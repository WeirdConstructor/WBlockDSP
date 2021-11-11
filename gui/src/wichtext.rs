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

use std::collections::HashMap;
use std::str::Chars;
use std::iter::Peekable;

pub trait WichTextValueSource {
    fn set(&self, key: &str, v: f32);
    fn value(&self, key: &str) -> f32;
    fn clamp(&self, key: &str, v: f32) -> f32;
    fn map_knob(&self, key: &str, v: f32) -> f32;
    fn step(&self, key: &str) -> f32;
    fn fmt(&self, key: &str, v: f32, buf: &mut [u8]) -> usize;
}

impl WichTextValueSource for RefCell<HashMap<String, (f32, f32, u8)>> {
    fn set(&self, key: &str, new_v: f32) {
        let mut create = false;

        if let Some((v, _, _)) = self.borrow_mut().get_mut(key) {
            *v = new_v;
        } else {
            create = true;
        }

        if create {
            self.borrow_mut().insert(key.to_string(), (new_v, 0.05, 2));
        }
    }

    fn map_knob(&self, key: &str, v: f32) -> f32 {
        v.clamp(0.0, 1.0)
    }

    fn clamp(&self, key: &str, v: f32) -> f32 {
        v.clamp(0.0, 1.0)
    }

    fn value(&self, key: &str) -> f32 {
        self.borrow().get(key).map(|v| v.0).unwrap_or(0.0)
    }

    fn step(&self, key: &str) -> f32 {
        self.borrow().get(key).map(|v| v.1).unwrap_or(0.05)
    }

    fn fmt(&self, key: &str, v: f32, buf: &mut [u8]) -> usize {
        use std::io::Write;
        let mut bw = std::io::BufWriter::new(buf);

        let prec =
            if let Some((_, _, prec)) = self.borrow().get(key) {
                *prec
            } else { 2 };

        match (match prec {
            0 => write!(bw, "{:5.0}", v),
            1 => write!(bw, "{:5.1}", v),
            2 => write!(bw, "{:5.2}", v),
            3 => write!(bw, "{:5.3}", v),
            4 => write!(bw, "{:6.4}", v),
            5 => write!(bw, "{:7.5}", v),
            _ => write!(bw, "{:8.6}", v),
        }) {
            Ok(_) => bw.buffer().len(),
            _     => 0,
        }
    }
}

pub trait WichTextDataSource {
    fn samples(&self) -> usize;
    fn sample(&self, i: usize) -> f32;
    fn generation(&self) -> usize { 1 }
}

impl WichTextDataSource for Vec<f32> {
    fn samples(&self) -> usize { self.len() }
    fn sample(&self, i: usize) -> f32 { self.get(i).copied().unwrap_or(0.0) }
}

impl WichTextDataSource for Rc<RefCell<Vec<f32>>> {
    fn samples(&self) -> usize { self.borrow().len() }
    fn sample(&self, i: usize) -> f32 { self.borrow().get(i).copied().unwrap_or(0.0) }
}

pub enum WichTextMessage {
    SetText(String),
    SetDataSource(String, Rc<dyn WichTextDataSource>),
    SetValueSource(Rc<dyn WichTextValueSource>),
}

#[derive(Debug, Clone)]
enum FragType {
    Text,
    Graph { key: String },
    Value { key: String },
}

impl FragType {
    fn is_value(&self) -> bool {
        if let FragType::Value { .. } = self {
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
pub struct WTFragment {
    typ:            FragType,
    font_size:      f32,
    color:          usize,
    color2:         usize,
    is_active:      bool,
    text:           String,
    cmd:            Option<String>,
    chars:          Vec<char>,
    ext_size_px:    (f32, f32),
    width_px:       f32,
    height_px:      f32,
    x:              f32,
}

impl WTFragment {
    fn new(font_size: f32) -> Self {
        Self {
            font_size,
            typ:            FragType::Text,
            color:          9,
            color2:         17,
            is_active:      false,
            text:           String::from(""),
            cmd:            None,
            chars:          vec![],
            ext_size_px:    (0.0, 0.0),
            width_px:       0.0,
            height_px:      0.0,
            x:              0.0,
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

        match self.typ {
            FragType::Value { .. } => {
                if self.is_active && self.height_px < 1.0 {
                    self.height_px = 40.0;
                }

                self.width_px =
                    self.width_px.max(
                        p.text_width(fs, true, &self.text) + 6.0);
                self.height_px += 2.0 * p.font_height(fs, true);
                self.ext_size_px.1 = p.font_height(fs, true);
            },
            FragType::Graph { .. } => {
                self.ext_size_px.0 = p.text_width(fs, true, &self.text) + 1.0;
                self.ext_size_px.1 = p.font_height(fs, true);
                self.height_px += self.ext_size_px.1;
                self.width_px   = self.ext_size_px.0.max(self.width_px);
            },
            FragType::Text => {
                self.width_px  = p.text_width(fs, true, &self.text) + 1.0;
                self.height_px = p.font_height(fs, true);
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum VAlign {
    Bottom,
    Top,
    Middle,
}

impl VAlign {
    fn from_char(c: char) -> Self {
        match c {
            't'     => VAlign::Top,
            'm'     => VAlign::Middle,
            'b' | _ => VAlign::Bottom,
        }
    }
}

#[derive(Debug, Clone)]
struct WTLine {
    frags:  Vec<WTFragment>,
    line_h: f32,
    line_y: f32,
    align:  VAlign,
}

impl WTLine {
    fn new() -> Self {
        Self {
            frags:  vec![],
            line_h: 0.0,
            line_y: 0.0,
            align:  VAlign::Bottom,
        }
    }

    fn add(&mut self, frag: WTFragment) { self.frags.push(frag); }

    fn finish(&mut self, default_h: f32, y: f32) -> f32 {
        let mut line_h = default_h;
        let mut x      = 0.0;

        for frag in &mut self.frags {
            line_h = line_h.max(frag.height_px);
            frag.x = x;
            x += frag.width_px;
        }

        self.line_h = line_h;
        self.line_y = y;

        line_h
    }
}

pub struct WichText {
    font:           Option<FontId>,
    font_mono:      Option<FontId>,
    style:          BlockCodeStyle,

    new_text:       Option<String>,
    lines:          Vec<WTLine>,
    full_h:         f32,

    zones:          Vec<(Rect, usize, usize)>,

    hover:          Option<(usize, usize)>,
    active:         Option<(usize, usize)>,
    drag:           Option<(f32, f32, f32, f32, f32, String)>,
    drag_delta:     Option<f32>,

    scroll:         (f32, f32),
    render:         (f32, f32),
    pan_pos:        Option<(f32, f32)>,

    data_sources:   HashMap<String, (usize, Vec<(f32, f32)>, Rc<dyn WichTextDataSource>)>,
    value_source:   Rc<dyn WichTextValueSource>,

    on_click:       Option<Box<dyn Fn(&mut Self, &mut State, Entity, usize, usize, &str)>>,
    on_value:       Option<Box<dyn Fn(&mut Self, &mut State, Entity, usize, usize, &str, f32)>>,
    on_hover:       Option<Box<dyn Fn(&mut Self, &mut State, Entity, bool, usize)>>,
}

fn parse_key(ci: &mut Peekable<Chars<'_>>) -> String {
    let mut key = String::from("");
    while let Some(c) = ci.peek().copied() {
        if c != ':' {
            ci.next();
            key.push(c);
        } else {
            break;
        }
    }
    key
}

fn parse_number<T: std::str::FromStr>(ci: &mut Peekable<Chars<'_>>, default: T) -> T {
    let mut s = String::from("");

    while let Some(c) = ci.peek().copied() {
        if c.is_ascii_digit() {
            ci.next();
            s.push(c);
        } else {
            break;
        }
    }

    s.parse::<T>().unwrap_or(default)
}

impl WichText {
    pub fn new(style: BlockCodeStyle) -> Self {
        Self {
            style,
            font:           None,
            font_mono:      None,

            new_text:       None,
            lines:          vec![],
            full_h:         0.0,
            data_sources:   HashMap::new(),
            value_source:   Rc::new(RefCell::new(HashMap::new())),

            zones:          vec![],

            scroll:         (0.0, 0.0),
            render:         (0.0, 0.0),
            pan_pos:        None,

            hover:          None,
            active:         None,
            drag:           None,
            drag_delta:     None,

            on_click:       None,
            on_value:       None,
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

    pub fn on_value<F>(mut self, on_value: F) -> Self
    where
        F: 'static + Fn(&mut Self, &mut State, Entity, usize, usize, &str, f32),
    {
        self.on_value = Some(Box::new(on_value));

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

        let mut cur_y = 0.0;

        for line in text.lines() {
            let mut frag_line = WTLine::new();
            let mut ci = line.chars().peekable();

            let mut cur_fragment  = WTFragment::new(self.style.font_size);
            let mut in_frag_start = false;
            let mut in_frag       = false;

            let mut align : u8 = 0;

            while let Some(c) = ci.next() {
                if in_frag_start {
                    match c {
                        'L' => {
                            match ci.next().unwrap_or('b') {
                                't'     => { align = 2; },
                                'm'     => { align = 1; },
                                'b' | _ => { align = 0; },
                            }
                        },
                        'v' => {
                            let key = parse_key(&mut ci);
                            cur_fragment.typ = FragType::Value { key };
                        },
                        'g' => {
                            let key = parse_key(&mut ci);
                            cur_fragment.typ = FragType::Graph { key };
                        },
                        'w' => {
                            let mut num = String::from("");
                            while let Some(c) = ci.peek().copied() {
                                if c.is_ascii_digit() {
                                    ci.next();
                                    num.push(c);
                                } else {
                                    break;
                                }
                            }

                            let w = num.parse::<f32>().unwrap_or(0.0);
                            cur_fragment.width_px      = w;
                            cur_fragment.ext_size_px.0 = w;
                        },
                        'h' => {
                            let h = parse_number::<f32>(&mut ci, 0.0);
                            cur_fragment.height_px     = h;
                            cur_fragment.ext_size_px.1 = h;
                        },
                        'c' => {
                            cur_fragment.color =
                                parse_number::<usize>(&mut ci, 0);
                        },
                        'C' => {
                            cur_fragment.color2 =
                                parse_number::<usize>(&mut ci, 0);
                        },
                        'f' => {
                            cur_fragment.font_size =
                                parse_number::<f32>(&mut ci, 0.0);
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

                                frag_line.add(
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

                                frag_line.add(
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

                frag_line.add(
                    std::mem::replace(
                        &mut cur_fragment,
                        WTFragment::new(self.style.font_size)));
            }


            let mut default_font_h = p.font_height(self.style.font_size, true);
            let line_h = frag_line.finish(default_font_h, cur_y);
            self.lines.push(frag_line);

            cur_y += line_h;
        }

        self.full_h = cur_y;
    }

    fn find_frag_idx_at(&self, x: f32, y: f32) -> Option<(usize, usize)> {
        for z in &self.zones {
            if z.0.is_inside(x, y) {
                return Some((z.1, z.2));
            }
        }

        None
    }

    fn clamp_scroll(&mut self, state: &mut State, mut dx: f32, mut dy: f32) -> (f32, f32) {
        let max_scroll =
            if self.full_h > self.render.1 { self.full_h - self.render.1 }
            else { 0.0 };

        if let Some((px, py)) = self.pan_pos {
            dx += state.mouse.cursorx - px;
            dy += state.mouse.cursory - py;
        }

        (self.scroll.0 + dx, (self.scroll.1 + dy).clamp(-max_scroll, 0.0))
    }

    fn drag_val(&self, mouse_y: f32) -> f32 {
        if let Some((_ox, oy, step, val, tmp, _key)) = &self.drag {
            val + ((oy - mouse_y) / 20.0) * step
        } else {
            0.0
        }
    }

    fn get(&self, line: usize, frag: usize) -> Option<&WTFragment> {
        self.lines.get(line)?.frags.get(frag)
    }

    fn get_mut(&mut self, line: usize, frag: usize) -> Option<&mut WTFragment> {
        self.lines.get_mut(line)?.frags.get_mut(frag)
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
                WichTextMessage::SetDataSource(key, src) => {
                    self.data_sources.insert(key.to_string(), (0, vec![], src.clone()));
                },
                WichTextMessage::SetValueSource(src) => {
                    self.value_source = src.clone();
                },
            }
        }

        if let Some(window_event) = event.message.downcast::<WindowEvent>() {
            match window_event {
                WindowEvent::MouseDown(btn) => {
                    let (x, y) = (state.mouse.cursorx, state.mouse.cursory);

                    if *btn == MouseButton::Middle {
                        self.pan_pos = Some((x, y));
                        state.capture(entity);

                    } else {
                        self.active = self.find_frag_idx_at(x, y);

                        if let Some((line, frag)) = self.active {
                            if let Some(FragType::Value { key }) =
                                self.get(line, frag).map(|f| &f.typ)
                            {
                                let s = self.value_source.step(&key);
                                let v = self.value_source.value(&key);

                                self.drag = Some((x, y, s, v, v, key.to_string()));
                                state.capture(entity);
                            }
                        }
                    }

                    state.insert_event(
                        Event::new(WindowEvent::Redraw)
                            .target(Entity::root()));
                },
                WindowEvent::MouseUp(btn) => {
                    let (x, y) = (state.mouse.cursorx, state.mouse.cursory);

                    let cur = self.find_frag_idx_at(x, y);

                    if *btn == MouseButton::Middle {
                        self.scroll = self.clamp_scroll(state, 0.0, 0.0);
                        self.pan_pos = None;

                    } else if self.active.is_some() && self.drag.is_some() {
                        let new_val = self.drag_val(y);
                        if let Some((_ox, _oy, _step, _val, _tmp, key)) =
                            self.drag.take()
                        {
                            let new_val =
                                self.value_source.clamp(&key, new_val);
                            self.value_source.set(&key, new_val);
                        }

                    } else if self.active.is_some() && self.active == cur {

                        if let Some((line, frag)) = self.active.take() {
                            if let Some(click) = self.on_click.take() {
                                if let Some(cmd) =
                                    self.get_mut(line, frag)
                                        .map(|f| f.cmd.take())
                                        .flatten()
                                {
                                    (click)(self, state, entity, line, frag, &cmd);

                                    if let Some(frag) = self.get_mut(line, frag) {
                                        frag.cmd = Some(cmd);
                                    }
                                }

                                self.on_click = Some(click);
                            }
                        }

                    }

                    state.release(entity);

                    self.active = None;
                    self.drag   = None;

                    state.insert_event(
                        Event::new(WindowEvent::Redraw)
                            .target(Entity::root()));
                },
                WindowEvent::MouseScroll(_x, y) => {
                    if self.pan_pos.is_none() {
                        self.scroll = self.clamp_scroll(state, 0.0, *y * 50.0);
                    }

                    state.insert_event(
                        Event::new(WindowEvent::Redraw)
                            .target(Entity::root()));
                },
                WindowEvent::MouseMove(x, y) => {
                    let old_hover = self.hover;
                    self.hover = self.find_frag_idx_at(*x, *y);

                    let d_val = self.drag_val(*y);

                    if let Some((_ox, _oy, _step, _val, tmp, key)) =
                        self.drag.as_mut()
                    {
                        *tmp = d_val;
                        state.insert_event(
                            Event::new(WindowEvent::Redraw)
                                .target(Entity::root()));
                    }

                    if self.pan_pos.is_some() {
                        state.insert_event(
                            Event::new(WindowEvent::Redraw)
                                .target(Entity::root()));

                    } else if old_hover != self.hover {
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

        p.rect_fill(self.style.bg_clr, pos.x, pos.y, pos.w, pos.h);

        let pos = pos.floor();
        let pos = pos.crop_right(10.0);

        let scroll_box = Rect {
            x: pos.w + pos.x,
            y: pos.y,
            w: 10.0,
            h: pos.h,
        };

        if let Some(new_text) = self.new_text.take() {
            self.parse(p, &new_text);
        }

        self.zones.clear();

        self.render = (pos.w, pos.h);

        let (scroll_x, scroll_y) = self.clamp_scroll(state, 0.0, 0.0);

        let mut y = 0.0;
        for (line_idx, WTLine { frags, line_h, line_y, align }) in
            self.lines.iter().enumerate()
        {
            for (frag_idx, frag) in frags.iter().enumerate() {
                let valign_offs =
                    match align {
                        VAlign::Middle => ((line_h - frag.height_px) * 0.5).floor(),
                        VAlign::Top    => 0.0,
                        VAlign::Bottom => line_h - frag.height_px,
                    };

                let frag_pos = Rect {
                    x: pos.x + frag.x,
                    y: pos.y + line_y + valign_offs + scroll_y,
                    w: frag.width_px,
                    h: frag.height_px,
                };

                let frag_pos = frag_pos.floor();

                if (frag_pos.y + frag_pos.h) < 0.0 {
                    continue;
                } else if frag_pos.y > self.render.1 {
                    continue;
                }

                let mut color =
                    self.style.block_clrs[
                        frag.color % self.style.block_clrs.len()];
                let orig_color = color;

                let mut color2 =
                    self.style.block_clrs[
                        frag.color2 % self.style.block_clrs.len()];

                if (self.active == self.hover || frag.typ.is_value())
                   && self.active == Some((line_idx, frag_idx)) {
                    color = self.style.port_select_clr;

                } else if self.hover == Some((line_idx, frag_idx)) {
                    p.rect_fill(
                        color, frag_pos.x, frag_pos.y, frag_pos.w, frag_pos.h);
                    color = self.style.bg_clr;
                }

                if let FragType::Graph { key } = &frag.typ {
                    let graph_h = frag_pos.h - frag.ext_size_px.1 - 2.0;
                    let graph_w = frag_pos.w - 2.0;
                    p.rect_stroke(
                        1.0,
                        color,
                        frag_pos.x + 0.5,
                        frag_pos.y + 1.5,
                        graph_w,
                        graph_h);

                    if let Some((gen, buf, data_src)) =
                        self.data_sources.get_mut(key)
                    {
                        if *gen != data_src.generation() {
                            buf.clear();
                            if data_src.samples() > 0 {
                                let xd =
                                    (graph_w - 1.0)
                                    / (data_src.samples() as f32 - 1.0);
                                let mut x = 0.0;
                                for i in 0..data_src.samples() {
                                    let s = 1.0 - data_src.sample(i).clamp(0.0, 1.0);
                                    buf.push((x, s * (graph_h - 2.0)));
                                    x += xd;
                                }
                            }
                            *gen = data_src.generation();
                        }

                        p.path_stroke(
                            1.0, color2,
                            &mut buf.iter().copied()
                                .map(|p| (
                                    (p.0 + frag_pos.x + 0.5).round(),
                                    (p.1 + frag_pos.y + 1.5).round() + 0.5)),
                            false);
                    }

                    p.label_mono(
                        frag.font_size,
                        0,
                        color,
                        frag_pos.x,
                        frag_pos.y + graph_h,
                        graph_w,
                        frag.ext_size_px.1,
                        &frag.text);

                } else if let FragType::Value { key } = &frag.typ {
                    let mut buf : [u8; 15] = [0; 15];
                    let v = self.value_source.value(key);
                    let v =
                        if let Some((_, _, _, _, tmp, drag_key)) = &self.drag {
                            if &key == &drag_key { *tmp }
                            else { v }
                        } else { v };

                    let v     = self.value_source.clamp(key, v);
                    let knb_v = self.value_source.map_knob(key, v);
                    let len   = self.value_source.fmt(key, v, &mut buf[..]);
                    let val_s = std::str::from_utf8(&buf[0..len]).unwrap();

                    let knob_h = frag_pos.h - 2.0 * frag.ext_size_px.1;

                    p.rect_stroke(
                        1.0,
                        color,
                        frag_pos.x + 1.5,
                        frag_pos.y + 1.5,
                        frag_pos.w - 3.0,
                        frag_pos.h - frag.ext_size_px.1 - 2.0);

                    p.rect_fill(
                        self.style.bg_clr,
                        frag_pos.x + 3.0,
                        frag_pos.y + 3.0,
                        frag_pos.w - 6.0,
                        frag_pos.h - frag.ext_size_px.1 - 5.0);

                    let factor = knob_h / 40.0;

                    if knob_h > 10.0 {
                        let r = (knob_h - (10.0 * factor));
                        p.arc_stroke(1.0, orig_color, r * 0.5,
                            std::f32::consts::PI * 0.6,
                            std::f32::consts::PI * (0.6 + 1.8),
                            (frag_pos.x + frag_pos.w * 0.5).floor(),
                            (frag_pos.y + 2.0 + knob_h * 0.5).floor());
                        p.arc_stroke(4.0 * factor, color2, r * 0.5,
                            std::f32::consts::PI * 0.6,
                            std::f32::consts::PI * (0.6 + 1.8 * knb_v),
                            (frag_pos.x + frag_pos.w * 0.5).floor(),
                            (frag_pos.y + 2.0 + knob_h * 0.5).floor());
                    }

                    p.label_mono(
                        frag.font_size,
                        1,
                        color2,
                        frag_pos.x,
                        frag_pos.y + knob_h,
                        frag_pos.w - 3.0,
                        frag.ext_size_px.1,
                        val_s);

                    p.label_mono(
                        frag.font_size,
                        0,
                        color,
                        frag_pos.x,
                        frag_pos.y + knob_h + frag.ext_size_px.1,
                        frag_pos.w, frag.ext_size_px.1,
                        &frag.text);

                } else {
                    p.label_mono(
                        frag.font_size,
                        -1,
                        color,
                        frag_pos.x, frag_pos.y, frag_pos.w, frag_pos.h,
                        &frag.text);
                }

                if frag.is_active {
                    self.zones.push((frag_pos, line_idx, frag_idx));
                }
            }
        }

        if self.full_h > self.render.1 {
            let scroll_marker_h = (scroll_box.h / 20.0).floor();
            let max_scroll = self.full_h - self.render.1;
            let marker_y =
                (scroll_y / max_scroll)
                    // XXX: +1.0 for the extra pixel padding!
                * ((scroll_marker_h + 1.0) - scroll_box.h);

            p.rect_stroke(1.0, self.style.border_clr,
                scroll_box.x + 0.5,
                scroll_box.y + 0.5,
                scroll_box.w - 1.0,
                scroll_box.h - 1.0);

            p.rect_fill(self.style.border_clr,
                scroll_box.x + 2.0,
                marker_y + 2.0,
                scroll_box.w - 4.0,
                scroll_marker_h - 3.0);
        }
    }
}
