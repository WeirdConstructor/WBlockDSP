// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This file is a part of HexoDSP. Released under GPL-3.0-or-later.
// See README.md and COPYING for details.

use crate::painter::*;
use crate::rect::*;
use crate::block_code_style::*;
use wblockdsp::{BlockView, BlockCodeView};

use tuix::*;
use femtovg::FontId;

use std::rc::Rc;
use std::cell::RefCell;

pub trait WichTextDataSource {
}

#[derive(Clone)]
pub enum WichTextMessage {
    SetText(String),
    SetDataSource(Rc<RefCell<dyn WichTextDataSource>>),
}

#[derive(Debug, Clone)]
pub struct WTFragment {
    color:      usize,
    is_active:  bool,
    text:       String,
    chars:      Vec<char>,
    width_px:   f32,
}

impl WTFragment {
    fn new() -> Self {
        Self {
            color:      9,
            is_active:  false,
            text:       String::from(""),
            chars:      vec![],
            width_px:   0.0,
        }
    }

    fn push_char(&mut self, c: char) {
        self.chars.push(c);
    }

    fn finish(&mut self, fs: f32, p: &mut FemtovgPainter) {
        if self.is_active {
            self.chars.insert(0, '[');
            self.chars.push(']');
            self.text = self.chars.iter().collect();
        }

        self.text = self.chars.iter().collect();

        self.width_px = p.text_width(fs, true, &self.text) + 1.0;
    }
}

pub struct WichText {
    font:           Option<FontId>,
    font_mono:      Option<FontId>,
    style:          BlockCodeStyle,

    new_text:       Option<String>,
    lines:          Vec<Vec<WTFragment>>,

    data_sources:   Vec<Rc<RefCell<dyn WichTextDataSource>>>,

    on_click:       Option<Box<dyn Fn(&mut Self, &mut State, Entity, MouseButton)>>,
    on_hover:       Option<Box<dyn Fn(&mut Self, &mut State, Entity, bool, usize)>>,
}

impl WichText {
    pub fn new(style: BlockCodeStyle) -> Self {
        Self {
            style,
            font:           None,
            font_mono:      None,

            new_text:       None,
            lines:          vec![],
            data_sources:   vec![],

            on_click:       None,
            on_hover:       None,
        }

    }

    pub fn on_click<F>(mut self, on_click: F) -> Self
    where
        F: 'static + Fn(&mut Self, &mut State, Entity, MouseButton),
    {
        self.on_click = Some(Box::new(on_click));

        self
    }

    pub fn on_hover<F>(mut self, on_hover: F) -> Self
    where
        F: 'static + Fn(&mut Self, &mut State, Entity, bool, usize),
    {
        self.on_hover = Some(Box::new(on_hover));

        self
    }

    fn parse(&mut self, p: &mut FemtovgPainter, text: &str) {
        self.lines.clear();

        for line in text.lines() {
            let mut frag_line = vec![];
            let mut ci = line.chars().peekable();

            let mut cur_fragment  = WTFragment::new();
            let mut in_frag_start = false;
            let mut in_frag       = false;

            while let Some(c) = ci.next() {
                if in_frag_start {
                    match c {
                        'c' => {
                            let mut num = String::from("");
                            while let Some(c) = ci.peek().copied() {
                                if c.is_ascii_digit() {
                                    ci.next();
                                    num.push(c);
                                } else {
                                    break;
                                }
                            }

                            let color = num.parse::<usize>().unwrap_or(0);
                            cur_fragment.color = color;
                        },
                        'a' => {
                            cur_fragment.is_active = true;
                        },
                        ':' => {
                            in_frag_start = false;
                            in_frag       = true;
                        },
                        _ => {
                            // ignore until ':'
                        },
                    }
                } else if in_frag {
                    match c {
                        ']' => {
                            if let Some(']') = ci.peek() {
                                ci.next();
                                cur_fragment.push_char(']');
                            } else {
                                cur_fragment.finish(self.style.font_size, p);

                                frag_line.push(
                                    std::mem::replace(
                                        &mut cur_fragment,
                                        WTFragment::new()));

                                in_frag = false;
                            }
                        },
                        _ => {
                            cur_fragment.push_char(c);
                        }
                    }

                } else {
                    match c {
                        '[' => {
                            if let Some('[') = ci.peek() {
                                ci.next();
                                cur_fragment.push_char('[');
                            } else {
                                cur_fragment.finish(self.style.font_size, p);

                                frag_line.push(
                                    std::mem::replace(
                                        &mut cur_fragment,
                                        WTFragment::new()));

                                in_frag_start = true;
                            }
                        },
                        _ => {
                            cur_fragment.push_char(c);
                        },
                    }
                }
            }

            if cur_fragment.chars.len() > 0 {
                cur_fragment.finish(self.style.font_size, p);

                frag_line.push(
                    std::mem::replace(
                        &mut cur_fragment,
                        WTFragment::new()));
            }

            self.lines.push(frag_line);
        }
    }
}

impl Widget for WichText {
    type Ret  = Entity;
    type Data = ();

    fn widget_name(&self) -> String {
        "wich-text".to_string()
    }

    fn on_build(&mut self, state: &mut State, entity: Entity) -> Self::Ret {
        entity.set_clip_widget(state, entity)
              .set_element(state, "wich_text")
    }

    fn on_event(&mut self, state: &mut State, entity: Entity, event: &mut Event) {
        if let Some(wt_msg) = event.message.downcast::<WichTextMessage>() {
            match wt_msg {
                WichTextMessage::SetText(text) => {
                    self.new_text = Some(text.clone());

                    state.insert_event(
                        Event::new(WindowEvent::Redraw)
                        .target(Entity::root()));
                },
                WichTextMessage::SetDataSource(src) => {
                },
            }
        }

        if let Some(window_event) = event.message.downcast::<WindowEvent>() {
            match window_event {
                WindowEvent::MouseDown(btn) => {
//                    let (x, y) = (state.mouse.cursorx, state.mouse.cursory);

//                    if *btn != MouseButton::Middle {
//                        self.m_down = self.find_pos_at_mouse(x, y);
//
//                        state.insert_event(
//                            Event::new(WindowEvent::Redraw)
//                                .target(Entity::root()));
//                    }
//
//                    state.capture(entity);
                },
                WindowEvent::MouseUp(btn) => {
                    let (x, y) = (state.mouse.cursorx, state.mouse.cursory);

//                    if *btn == MouseButton::Middle {
//                        if let Some(tmp_shift_offs) = self.tmp_shift_offs.take() {
//                            self.shift_offs.0 += tmp_shift_offs.0;
//                            self.shift_offs.1 += tmp_shift_offs.1;
//                        }
//
//                        state.insert_event(
//                            Event::new(WindowEvent::Redraw)
//                                .target(Entity::root()));
//                    } else {
//                        state.insert_event(
//                            Event::new(WindowEvent::Redraw)
//                                .target(Entity::root()));
//                    }

                    state.release(entity);
//                    self.m_down = None;
                },
                WindowEvent::MouseMove(x, y) => {
//                    if state.mouse.middle.state == MouseButtonState::Pressed {
//                        self.tmp_shift_offs =
//                            Some((
//                                *x - state.mouse.middle.pos_down.0,
//                                *y - state.mouse.middle.pos_down.1
//                            ));
//
//                        state.insert_event(
//                            Event::new(WindowEvent::Redraw)
//                                .target(Entity::root()));
//                    } else {
//                        let old_hover = self.hover;
//                        let mut found = false;
//
//                        self.hover = self.find_area_at_mouse(*x, *y);
//
//                        if old_hover != self.hover {
//                            state.insert_event(
//                                Event::new(WindowEvent::Redraw)
//                                    .target(Entity::root()));
//                        }
//                    }
                },
                _ => {},
            }
        }
    }

    fn on_draw(&mut self, state: &mut State, entity: Entity, canvas: &mut Canvas) {
        let bounds = state.data.get_bounds(entity);
        if self.font.is_none() {
            self.font      = Some(canvas.add_font_mem(std::include_bytes!("font.ttf")).expect("can load font"));
            self.font_mono = Some(canvas.add_font_mem(std::include_bytes!("font_mono.ttf")).expect("can load font"));
        }

        let p = &mut FemtovgPainter {
            canvas:     canvas,
            font:       self.font.unwrap(),
            font_mono:  self.font_mono.unwrap(),
        };

        let pos : Rect = bounds.into();
        let pos = pos.floor();

        p.rect_fill(self.style.bg_clr, pos.x, pos.y, pos.w, pos.h);

        if let Some(new_text) = self.new_text.take() {
            self.parse(p, &new_text);
            println!("PARSE {:?}", self.lines);
        }

        let mut y = 0.0;
        let line_h = p.font_height(self.style.font_size, true);
        for line in &self.lines {
            let mut x = 0.0;
            for frag in line {
                p.label_mono(
                    self.style.font_size,
                    -1,
                    self.style.block_clrs[
                        frag.color % self.style.block_clrs.len()],
                    pos.x + x,
                    pos.y + y,
                    frag.width_px,
                    line_h,
                    &frag.text);

                x += frag.width_px;
            }

            y += line_h;
        }
    }
}
