// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This file is a part of HexoDSP. Released under GPL-3.0-or-later.
// See README.md and COPYING for details.

use crate::painter::*;
use crate::rect::*;
use wblockdsp::{BlockView, BlockCodeView};

use tuix::*;
use femtovg::FontId;

use std::rc::Rc;
use std::cell::RefCell;

pub struct DummyBlockCode { }

impl DummyBlockCode {
    pub fn new() -> Self {
        Self { }
    }
}

impl BlockCodeView for DummyBlockCode {
    fn area_size(&self, id: usize) -> (usize, usize) {
        (16, 16)
    }

    fn block_at(&self, id: usize, x: usize, y: usize) -> Option<&dyn BlockView> {
        None
    }

    fn origin_at(&self, id: usize, x: usize, y: usize)
        -> Option<(usize, usize)>
    {
        None
    }
}

macro_rules! hxclr {
    ($i: expr) => {
        (
            ($i >> 16 & 0xFF) as f32 / 255.0,
            ($i >> 8  & 0xFF) as f32 / 255.0,
            ($i       & 0xFF) as f32 / 255.0,
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BlockCodeStyle {
    bg_clr:             (f32, f32, f32), // UI_ACCENT_BG1_CLR
    block_bg_hover_clr: (f32, f32, f32), // UI_ACCENT_CLR
    block_bg_clr:       (f32, f32, f32), // UI_ACCENT_BG2_CLR
    border_hover_clr:   (f32, f32, f32), // UI_HLIGHT_CLR
    border_clr:         (f32, f32, f32), // UI_PRIM_CLR
    port_select_clr:    (f32, f32, f32), // UI_SELECT_CLR
    grid_marker_clr:    (f32, f32, f32), // UI_ACCENT_DARK_CLR
}

impl BlockCodeStyle {
    pub fn new_default() -> Self {
        Self {
            bg_clr:             hxclr!(0x111920),
            block_bg_hover_clr: hxclr!(0x922f93),
            block_bg_clr:       hxclr!(0x192129),
            border_hover_clr:   hxclr!(0xecf9ce),
            border_clr:         hxclr!(0x03fdcb),
            port_select_clr:    hxclr!(0xd73988),
            grid_marker_clr:    hxclr!(0x1e333d),
        }
    }
}

#[derive(Clone)]
pub enum BlockCodeMessage {
    SetCode(Rc<RefCell<dyn BlockCodeView>>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BlockPos {
    Block { id: usize, x: usize, y: usize, row: usize, col: usize },
    Cell  { id: usize, x: usize, y: usize },
}

pub struct BlockCode {
    font_size:      f32,
    font:           Option<FontId>,
    font_mono:      Option<FontId>,
    code:           Rc<RefCell<dyn BlockCodeView>>,
    style:          BlockCodeStyle,

    block_size:     f32,

    areas:          Vec<Vec<(usize, Rect)>>,
    hover:          Option<(usize, usize, usize, usize)>,

    m_down:         Option<BlockPos>,

    on_change:      Option<Box<dyn Fn(&mut Self, &mut State, Entity, (usize, usize))>>,
    on_expand:      Option<Box<dyn Fn(&mut Self, &mut State, Entity, usize)>>,
    on_click:       Option<Box<dyn Fn(&mut Self, &mut State, Entity, BlockPos)>>,
    on_hover:       Option<Box<dyn Fn(&mut Self, &mut State, Entity, bool, usize)>>,
}

impl BlockCode {
    pub fn new(style: BlockCodeStyle) -> Self {
        Self {
            font_size:      14.0,
            font:           None,
            font_mono:      None,
            code:           Rc::new(RefCell::new(DummyBlockCode::new())),

            style,

            block_size:     30.0,

            areas:          vec![],
            hover:          None,
            m_down:         None,

            on_change:      None,
            on_expand:      None,
            on_click:       None,
            on_hover:       None,
        }
    }

    pub fn on_change<F>(mut self, on_change: F) -> Self
    where
        F: 'static + Fn(&mut Self, &mut State, Entity, (usize, usize)),
    {
        self.on_change = Some(Box::new(on_change));

        self
    }

    pub fn on_expand<F>(mut self, on_expand: F) -> Self
    where
        F: 'static + Fn(&mut Self, &mut State, Entity, usize),
    {
        self.on_expand = Some(Box::new(on_expand));

        self
    }

    pub fn on_click<F>(mut self, on_click: F) -> Self
    where
        F: 'static + Fn(&mut Self, &mut State, Entity, BlockPos),
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

    pub fn reset_areas(&mut self) {
        for a in self.areas.iter_mut() {
            a.clear();
        }
    }

    pub fn store_area_pos(&mut self, area_id: usize, level: usize, pos: Rect) {
        if level >= self.areas.len() {
            self.areas.resize_with(area_id + 1, || vec![]);
        }

        self.areas[level].push((area_id, pos));
    }

    pub fn draw_area(&mut self, p: &mut FemtovgPainter, area_id: usize, pos: Rect, level: usize) {
        p.clip_region(pos.x, pos.y, pos.w, pos.h);

        let block_h = self.block_size;
        let block_w = block_h * 2.0;

        let cols = (pos.w / block_w).ceil() as usize;
        let rows = (pos.h / block_h).ceil() as usize;

        p.rect_fill(self.style.bg_clr, pos.x, pos.y, pos.w, pos.h);

        for row in 0..rows {
            for col in 0..cols {
                let x = col as f32 * block_w;
                let y = row as f32 * block_h;

                let marker_px = (block_h * 0.2).floor();
                draw_markers(
                    p, self.style.grid_marker_clr,
                    pos.x + x, pos.y + y,
                    block_w, block_h, marker_px);
            }
        }

        let mut next_areas = vec![];

        let mut lbl_buf : [u8; 20] = [0; 20];

        for row in 0..rows {
            for col in 0..cols {
                let x = col as f32 * block_w;
                let y = row as f32 * block_h;

                let mut hover_here =
                    if let Some(hover) = self.hover {
                        hover.0 == area_id && col == hover.1 && row == hover.2
                    } else {
                        false
                    };

                let mut hover_row = -1;
                let mut hover_col = -1;

                if let Some((area, x, y, subcol)) = self.hover {
                    if area == area_id {
                        if let Some((bx, by)) =
                            self.code.borrow().origin_at(area, x, y)
                        {
                            hover_row = (y - by) as i32;
                            hover_col = subcol as i32;
                            hover_here = bx == col && by == row;
                        }
                    }
                }

                let bg_color =
                    if hover_here { self.style.block_bg_hover_clr }
                    else { self.style.block_bg_clr };
                let border_color =
                    if hover_here { self.style.border_hover_clr }
                    else { self.style.border_clr };

                if let Some(block) =
                    self.code.borrow().block_at(area_id, col, row)
                {
                    let w = block_w;
                    let h = block.rows() as f32 * block_h;

                    p.rect_fill(bg_color, pos.x + x, pos.y + y, w, h);

                    p.rect_stroke(
                        2.0, border_color,
                        pos.x + x + 1.0,
                        pos.y + y + 1.0,
                        w - 2.0, h - 2.0);

                    let hole_px = (0.6 * block_h).ceil();

                    let len = block.label(&mut lbl_buf[..]);
                    let val_s = std::str::from_utf8(&lbl_buf[0..len]).unwrap();
                    p.label(
                        self.block_size * 0.5,
                        0, self.style.border_clr,
                        pos.x + x, pos.y + y, w, h, val_s);

//                                    let fs =
//                                        calc_font_size_from_text(
//                                            p, name_lbl, fs, maxwidth);

                    for i in 0..block.rows() {
                        if block.has_input(i) {
                            let row = i as f32 * block_h;
                            p.rect_fill(
                                bg_color,
                                pos.x + x,
                                pos.y + y + row
                                + ((block_h - hole_px) * 0.5).floor(),
                                3.0,
                                hole_px);

                            let len = block.input_label(i, &mut lbl_buf[..]);
                            let val_s = std::str::from_utf8(&lbl_buf[0..len]).unwrap();
                            p.label(
                                self.block_size * 0.4,
                                -1,
                                self.style.border_clr,
                                pos.x + x,
                                pos.y + row + y - 1.0,
                                (block_w * 0.5).floor(),
                                block_h,
                                val_s);

                            if hover_here
                               && hover_col == 0
                               && hover_row == (i as i32)
                            {
                                let sel_block_w = (block_w * 0.5 * 0.8).floor();
                                let sel_block_h = (block_h * 0.8).floor();

                                p.rect_stroke(
                                    4.0, self.style.port_select_clr,
                                    (pos.x + x
                                     + ((block_w * 0.5 - sel_block_w) * 0.5)).floor(),
                                    (pos.y + row + y
                                     + ((block_h - sel_block_h) * 0.5)).floor(),
                                    sel_block_w,
                                    sel_block_h);
                            }
                        }

                        if block.has_output(i) {
                            let row = i as f32 * block_h;
                            p.rect_fill(
                                bg_color,
                                pos.x + x + w - 3.0,
                                pos.y + y + row
                                + ((block_h - hole_px) * 0.5).floor(),
                                3.0,
                                hole_px);

                            let len = block.output_label(i, &mut lbl_buf[..]);
                            let val_s = std::str::from_utf8(&lbl_buf[0..len]).unwrap();
                            p.label(
                                self.block_size * 0.3,
                                1,
                                self.style.border_clr,
                                (pos.x + x + (block_w * 0.5)).floor(),
                                pos.y + row + y - 1.0,
                                (block_w * 0.5).floor(),
                                block_h,
                                val_s);

                            if hover_here
                               && hover_col == 1
                               && hover_row == (i as i32)
                            {
                                let sel_block_w = (block_w * 0.5 * 0.8).floor();
                                let sel_block_h = (block_h * 0.8).floor();

                                p.rect_stroke(
                                    4.0, self.style.port_select_clr,
                                    (pos.x + x + (block_w * 0.5)
                                     + ((block_w * 0.5 - sel_block_w) * 0.5)).floor(),
                                    (pos.y + row + y
                                     + ((block_h - sel_block_h) * 0.5)).floor(),
                                    sel_block_w,
                                    sel_block_h);
                            }
                        }
                    }

                    if let Some(cont_id) = block.contains(0) {
                        let (area_w, area_h) =
                            self.code.borrow().area_size(cont_id);
                        let bpos = Rect {
                            x: pos.x + x, // + border
                            y: pos.y + y + h,
                            w: (area_w as f32 * block_w + block_w * 0.3).floor(),
                            h: (area_h as f32 * block_h + block_w * 0.3).floor(),
                        };

                        next_areas.push((cont_id, bpos, border_color, bg_color));

                        if let Some(cont_id) = block.contains(1) {
                            let (area_w, area_h) =
                                self.code.borrow().area_size(cont_id);
                            let bpos = Rect {
                                x: bpos.x,
                                y: bpos.y + bpos.h,
                                w: (area_w as f32 * block_w + block_w * 0.3).floor(),
                                h: (area_h as f32 * block_h + block_w * 0.3).floor(),
                            };

                            next_areas.push((cont_id, bpos, border_color, bg_color));
                        }
                    }

                } else if hover_here {
                    p.rect_stroke(
                        2.0, self.style.border_hover_clr,
                        pos.x + x + 1.0,
                        pos.y + y + 1.0,
                        block_w - 2.0, block_h - 2.0);
                }
            }
        }

        for cont_area in next_areas.into_iter() {
            let (cont_id, pos, border_color, bg_color) = cont_area;

            let (area_w, area_h) = self.code.borrow().area_size(cont_id);
            let apos = Rect {
                x: pos.x + 4.0, // + border
                y: pos.y + 4.0,
                w: pos.w,
                h: pos.h,
            };
            p.rect_fill(
                border_color,
                apos.x - 4.0, apos.y - 4.0, apos.w + 8.0, apos.h + 8.0);
            p.rect_fill(
                self.style.bg_clr,
                apos.x - 2.0, apos.y - 2.0, apos.w + 4.0, apos.h + 4.0);
            p.rect_fill(
                bg_color,
                (pos.x + block_w * 0.25).floor(),
                pos.y - 2.0,
                (block_w * 0.5).floor(),
                8.0);

            self.store_area_pos(cont_id, level + 1, Rect {
                x: apos.x - 4.0,
                y: apos.y - 4.0,
                w: apos.w + 8.0,
                h: apos.h + 8.0,
            });
            self.draw_area(p, cont_id, apos, level + 1);
        }

        p.reset_clip_region();
    }

    fn find_area_at(&self, x: f32, y: f32) -> Option<(usize, usize, usize, usize)> {
        let block_h = self.block_size;
        let block_w = block_h * 2.0;

        for lvl in self.areas.iter().rev() {
            for a in lvl.iter() {
                let (id, pos) = *a;

                if pos.is_inside(x, y) {
                    let xo = x - pos.x;
                    let yo = y - pos.y;
                    let xi = (xo / block_w).floor() as usize;
                    let yi = (yo / block_h).floor() as usize;

                    let sub_col =
                        if (xo - xi as f32 * block_w) > (block_w * 0.5) {
                            1
                        } else {
                            0
                        };

                    return Some((a.0, xi, yi, sub_col));
                }
            }
        }

        None
    }

    fn find_pos_at(&self, x: f32, y: f32) -> Option<BlockPos> {
        if let Some((area, x, y, subcol)) = self.find_area_at(x, y) {
            if let Some((ox, oy)) =
                self.code.borrow().origin_at(area, x, y)
            {
                let row = y - oy;
                Some(BlockPos::Block { id: area, x, y, col: subcol, row })
            } else {
                Some(BlockPos::Cell { id: area, x, y })
            }
        } else {
            None
        }
    }
}

fn draw_markers(p: &mut FemtovgPainter, clr: (f32, f32, f32), x: f32, y: f32, block_w: f32, block_h: f32, marker_px: f32) {
    p.path_stroke(
        1.0,
        clr,
        &mut ([
            (x,             y + marker_px),
            (x,             y),
            (x + marker_px, y),
        ].iter().copied()
         .map(|p| (p.0.floor() + 0.5, p.1.floor() + 0.5))), false);

    p.path_stroke(
        1.0,
        clr,
        &mut ([
            (block_w + x - marker_px, y),
            (block_w + x,             y),
            (block_w + x,             y + marker_px),
        ].iter().copied()
         .map(|p| (p.0.floor() - 0.5, p.1.floor() + 0.5))), false);

    p.path_stroke(
        1.0,
        clr,
        &mut ([
            (x,             block_h + y - marker_px),
            (x,             block_h + y),
            (x + marker_px, block_h + y),
        ].iter().copied()
         .map(|p| (p.0.floor() + 0.5, p.1.floor() - 0.5))), false);

    p.path_stroke(
        1.0,
        clr,
        &mut ([
            (block_w + x - marker_px, block_h + y),
            (block_w + x,             block_h + y),
            (block_w + x,             block_h + y - marker_px),
        ].iter().copied()
         .map(|p| (p.0.floor() - 0.5, p.1.floor() - 0.5))), false);
}

impl Widget for BlockCode {
    type Ret  = Entity;
    type Data = ();

    fn widget_name(&self) -> String {
        "block-code".to_string()
    }

    fn on_build(&mut self, state: &mut State, entity: Entity) -> Self::Ret {
        entity.set_clip_widget(state, entity)
              .set_element(state, "block_code")
    }

    fn on_event(&mut self, state: &mut State, entity: Entity, event: &mut Event) {
        if let Some(grid_msg) = event.message.downcast::<BlockCodeMessage>() {
            match grid_msg {
                BlockCodeMessage::SetCode(code) => {
                    self.code = code.clone();
                    state.insert_event(
                        Event::new(WindowEvent::Redraw)
                        .target(Entity::root()));
                },
            }
        }

        if let Some(window_event) = event.message.downcast::<WindowEvent>() {
            match window_event {
                WindowEvent::MouseDown(MouseButton::Left) => {
                    let (x, y) = (state.mouse.cursorx, state.mouse.cursory);
                    self.m_down = self.find_pos_at(x, y);

//                    self.
//                    self.drag = true;
//                    self.drag_src_idx = self.xy2pos(state, entity, x, y);
//
//                    if let Some((inputs, _)) = self.drag_src_idx {
//                        if inputs {
//                            if self.items.0.len() == 1 {
//                                self.drag_src_idx = Some((false, 0));
//                            }
//                        } else {
//                            if self.items.1.len() == 1 {
//                                self.drag_src_idx = Some((true, 0));
//                            }
//                        }
//                    }
//
                    state.capture(entity);
                    state.insert_event(
                        Event::new(WindowEvent::Redraw)
                            .target(Entity::root()));
                },
                WindowEvent::MouseUp(MouseButton::Left) => {
                    let (x, y) = (state.mouse.cursorx, state.mouse.cursory);

                    let m_up = self.find_pos_at(x, y);
                    if m_up == self.m_down {
                        println!("CLICK: {:?}", m_up);
                        if let Some(pos) = m_up {
                            if let Some(cb) = self.on_click.take() {
                                (*cb)(self, state, entity, pos);
                                self.on_click = Some(cb);
                            }
                        }
                    } else {
                        println!("DRAG: {:?} => {:?}", self.m_down, m_up);
                    }

                    self.m_down = None;

//                    if let Some((_drag, con)) = self.get_current_con() {
//                        self.con = Some(con);
//
//                        if let Some(callback) = self.on_change.take() {
//                            (callback)(self, state, entity, con);
//                            self.on_change = Some(callback);
//                        }
//                    } else {
//                        self.con = None;
//                    }
//
//                    self.drag = false;
//                    self.drag_src_idx = None;
//
                    state.release(entity);
                    state.insert_event(
                        Event::new(WindowEvent::Redraw)
                            .target(Entity::root()));
                },
                WindowEvent::MouseMove(x, y) => {
                    let old_hover = self.hover;
                    let mut found = false;

                    self.hover = self.find_area_at(*x, *y);

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

        self.reset_areas();

        self.store_area_pos(0, 0, pos);
        self.draw_area(p, 0, pos, 0);
    }
}
