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
    font_size:  f32,
    color:      usize,
    is_active:  bool,
    text:       String,
    cmd:        Option<String>,
    chars:      Vec<char>,
    width_px:   f32,
    height_px:  f32,
}

impl WTFragment {
    fn new(font_size: f32) -> Self {
        Self {
            font_size,
            color:      9,
            is_active:  false,
            text:       String::from(""),
            cmd:        None,
            chars:      vec![],
            width_px:   0.0,
            height_px:  0.0,
        }
    }

    fn push_char(&mut self, c: char) {
        self.chars.push(c);
    }

    fn finish(&mut self, p: &mut FemtovgPainter) {
        if self.is_active {
            self.cmd = Some(self.chars.iter().collect());

            self.chars.insert(0, '[');
            self.chars.push(']');
            self.text = self.chars.iter().collect();
        }

        self.text = self.chars.iter().collect();

        let fs = self.font_size;

        self.width_px  = p.text_width(fs, true, &self.text) + 1.0;
        self.height_px = p.font_height(fs, true);
    }
}

pub struct WichText {
    font:           Option<FontId>,
    font_mono:      Option<FontId>,
    style:          BlockCodeStyle,

    new_text:       Option<String>,
    lines:          Vec<Vec<WTFragment>>,

    zones:          Vec<(Rect, usize, usize)>,

    hover:          Option<(usize, usize)>,
    active:         Option<(usize, usize)>,

    data_sources:   Vec<Rc<RefCell<dyn WichTextDataSource>>>,

    on_click:       Option<Box<dyn Fn(&mut Self, &mut State, Entity, usize, usize, &str)>>,
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

            zones:          vec![],

            hover:          None,
            active:         None,

            on_click:       None,
            on_hover:       None,
        }

    }

    pub fn on_click<F>(mut self, on_click: F) -> Self
    where
        F: 'static + Fn(&mut Self, &mut State, Entity, usize, usize, &str),
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

            let mut cur_fragment  = WTFragment::new(self.style.font_size);
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
                        'f' => {
                            let mut num = String::from("");
                            while let Some(c) = ci.peek().copied() {
                                if c.is_ascii_digit() {
                                    ci.next();
                                    num.push(c);
                                } else {
                                    break;
                                }
                            }

                            let fs = num.parse::<f32>().unwrap_or(0.0);
                            cur_fragment.font_size = fs;
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
                                cur_fragment.finish(p);

                                frag_line.push(
                                    std::mem::replace(
                                        &mut cur_fragment,
                                        WTFragment::new(self.style.font_size)));

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
                                cur_fragment.finish(p);

                                frag_line.push(
                                    std::mem::replace(
                                        &mut cur_fragment,
                                        WTFragment::new(self.style.font_size)));

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
                cur_fragment.finish(p);

                frag_line.push(
                    std::mem::replace(
                        &mut cur_fragment,
                        WTFragment::new(self.style.font_size)));
            }

            self.lines.push(frag_line);
        }
    }

    fn find_frag_idx_at(&self, x: f32, y: f32) -> Option<(usize, usize)> {
        for z in &self.zones {
            if z.0.is_inside(x, y) {
                return Some((z.1, z.2));
            }
        }

        None
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
                    let (x, y) = (state.mouse.cursorx, state.mouse.cursory);

                    self.active = self.find_frag_idx_at(x, y);
                    if self.active.is_some() {
                        state.capture(entity);
                    }

                    state.insert_event(
                        Event::new(WindowEvent::Redraw)
                            .target(Entity::root()));
                },
                WindowEvent::MouseUp(btn) => {
                    let (x, y) = (state.mouse.cursorx, state.mouse.cursory);

                    let cur = self.find_frag_idx_at(x, y);

                    if self.active.is_some() && self.active == cur {

                        if let Some((line, frag)) = self.active.take() {
                            if let Some(click) = self.on_click.take() {
                                if let Some(cmd) =
                                    self.lines[line][frag].cmd.take()
                                {
                                    (click)(self, state, entity, line, frag, &cmd);

                                    self.lines[line][frag].cmd = Some(cmd);
                                }

                                self.on_click = Some(click);
                            }
                        }

                        state.release(entity);
                    }

                    self.active = None;

                    state.insert_event(
                        Event::new(WindowEvent::Redraw)
                            .target(Entity::root()));
                },
                WindowEvent::MouseMove(x, y) => {
                    let old_hover = self.hover;
                    self.hover = self.find_frag_idx_at(*x, *y);

                    if old_hover != self.hover {
                        state.insert_event(
                            Event::new(WindowEvent::Redraw)
                                .target(Entity::root()));
                    }
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

        self.zones.clear();

        let mut y = 0.0;
        for (line_idx, line) in self.lines.iter().enumerate() {
            let mut line_h = p.font_height(self.style.font_size, true);

            let mut x = 0.0;
            for (frag_idx, frag) in line.iter().enumerate() {
                let frag_pos = Rect {
                    x: pos.x + x,
                    y: pos.y + y,
                    w: frag.width_px,
                    h: frag.height_px,
                };

                let mut color =
                    self.style.block_clrs[
                        frag.color % self.style.block_clrs.len()];

                if self.active == self.hover
                   && self.active == Some((line_idx, frag_idx)) {
                    color = self.style.port_select_clr;

                } else if self.hover == Some((line_idx, frag_idx)) {
                    p.rect_fill(
                        color, frag_pos.x, frag_pos.y, frag_pos.w, frag_pos.h);
                    color = self.style.bg_clr;
                }

                p.label_mono(
                    frag.font_size,
                    -1,
                    color,
                    frag_pos.x, frag_pos.y, frag_pos.w, frag_pos.h,
                    &frag.text);

                if frag.is_active {
                    self.zones.push((Rect {
                        x: pos.x + x,
                        y: pos.y + y,
                        w: frag.width_px,
                        h: frag.height_px,
                    }, line_idx, frag_idx));
                }

                line_h = line_h.max(frag.height_px);

                x += frag.width_px;
            }

            y += line_h;
        }
    }
}
