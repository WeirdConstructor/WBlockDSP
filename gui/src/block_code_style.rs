// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This file is a part of HexoDSP. Released under GPL-3.0-or-later.
// See README.md and COPYING for details.

macro_rules! hxclr {
    ($i: expr) => {
        (
            ($i >> 16 & 0xFF) as f32 / 255.0,
            ($i >> 8  & 0xFF) as f32 / 255.0,
            ($i       & 0xFF) as f32 / 255.0,
        )
    }
}

#[derive(Debug, Clone)]
pub struct BlockCodeStyle {
    pub font_size:          f32,
    pub bg_clr:             (f32, f32, f32), // UI_ACCENT_BG1_CLR
    pub block_bg_hover_clr: (f32, f32, f32), // UI_ACCENT_CLR
    pub block_bg_clr:       (f32, f32, f32), // UI_ACCENT_BG2_CLR
    pub border_hover_clr:   (f32, f32, f32), // UI_HLIGHT_CLR
    pub border_clr:         (f32, f32, f32), // UI_PRIM_CLR
    pub port_select_clr:    (f32, f32, f32), // UI_SELECT_CLR
    pub grid_marker_clr:    (f32, f32, f32), // UI_ACCENT_DARK_CLR
    pub with_markers:       bool,
    pub block_clrs:         Vec<(f32, f32, f32)>,
}

impl BlockCodeStyle {
    pub fn new_default() -> Self {
        let block_clrs = vec![
            hxclr!(0x922f93), // 0
            hxclr!(0x862b37),
            hxclr!(0xb45745),
            hxclr!(0x835933),
            hxclr!(0xa69b64),
            hxclr!(0xbec8a6),
            hxclr!(0x346c38), // 6
            hxclr!(0x1fb349),
            hxclr!(0x4cdb80),
            hxclr!(0x59bca3),
            hxclr!(0x228f9d),
            hxclr!(0x03b5e7),
            hxclr!(0x3b5eca), // 12
            hxclr!(0x594fa1),
            hxclr!(0xc2b2eb),
            hxclr!(0xac70fa),
            hxclr!(0x9850a9),
            hxclr!(0xdc4fc1), // 17
        ];

        Self {
            font_size:          14.0,
            bg_clr:             hxclr!(0x111920),
            block_bg_hover_clr: hxclr!(0x922f93),
            block_bg_clr:       hxclr!(0x192129),
            border_hover_clr:   hxclr!(0xecf9ce),
            border_clr:         hxclr!(0x03fdcb),
            port_select_clr:    hxclr!(0xd73988),
            grid_marker_clr:    hxclr!(0x1e333d),
            with_markers:       false,
            block_clrs,
        }
    }
}

