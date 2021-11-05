use crate::block_view::{BlockView, BlockCodeView};
use std::rc::Rc;
use std::cell::RefCell;

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Block {
    rows:     usize,
    contains: (Option<usize>, Option<usize>),
    expanded: bool,
    typ:      String,
    lbl:      String,
    inputs:   Vec<Option<String>>,
    outputs:  Vec<Option<String>>,
}

impl BlockView for Block {
    fn rows(&self) -> usize { self.rows }
    fn contains(&self, idx: usize) -> Option<usize> {
        if idx == 0 { self.contains.0 }
        else { self.contains.1 }
    }
    fn expanded(&self) -> bool { true }
    fn label(&self, buf: &mut [u8]) -> usize {
        use std::io::Write;
        let mut bw = std::io::BufWriter::new(buf);
        match write!(bw, "{}", self.lbl) {
            Ok(_) => bw.buffer().len(),
            _ => 0,
        }
    }
    fn has_input(&self, idx: usize) -> bool {
        self.inputs.get(idx).map(|s| s.is_some()).unwrap_or(false)
    }
    fn has_output(&self, idx: usize) -> bool {
        self.outputs.get(idx).map(|s| s.is_some()).unwrap_or(false)
    }
    fn input_label(&self, idx: usize, buf: &mut [u8]) -> usize {
        use std::io::Write;
        if let Some(lbl_opt) = self.inputs.get(idx) {
            if let Some(lbl) = lbl_opt {
                let mut bw = std::io::BufWriter::new(buf);
                match write!(bw, "{}", lbl) {
                    Ok(_) => bw.buffer().len(),
                    _ => 0,
                }
            } else { 0 }
        } else { 0 }
    }
    fn output_label(&self, idx: usize, buf: &mut [u8]) -> usize {
        use std::io::Write;
        if let Some(lbl_opt) = self.outputs.get(idx) {
            if let Some(lbl) = lbl_opt {
                let mut bw = std::io::BufWriter::new(buf);
                match write!(bw, "{}", lbl) {
                    Ok(_) => bw.buffer().len(),
                    _ => 0,
                }
            } else { 0 }
        } else { 0 }
    }
}


#[derive(Debug, Clone)]
pub struct BlockArea {
    blocks:     HashMap<(usize, usize), Box<Block>>,
    origin_map: HashMap<(usize, usize), (usize, usize)>,
    size:       (usize, usize),
}

impl BlockArea {
    fn new(w: usize, h: usize) -> Self {
        Self {
            blocks:     HashMap::new(),
            origin_map: HashMap::new(),
            size:       (w, h),
        }
    }

    fn set_block_at(&mut self, x: usize, y: usize, block: Box<Block>) {
        self.blocks.insert((x, y), block);
        self.update_origin_map();
        self.update_size();
    }

    fn update_size(&mut self) {
        let mut min_w = 1;
        let mut min_h = 1;

        for ((ox, oy), _) in &self.origin_map {
            if min_w < (ox + 1) { min_w = ox + 1; }
            if min_h < (oy + 1) { min_h = oy + 1; }
        }

        if self.size.0 < min_w { self.size.0 = min_w; }
        if self.size.1 < min_h { self.size.0 = min_h; }
    }

    fn update_origin_map(&mut self) {
        self.origin_map.clear();

        for ((ox, oy), block) in &self.blocks {
            for r in 0..block.rows {
                self.origin_map.insert((*ox, *oy + r), (*ox, *oy));
            }
        }
    }

    fn check_space_at(&self, x: usize, y: usize, rows: usize) -> bool {
        for i in 0..rows {
            let yo = y + i;

            if self.origin_map.get(&(x, yo)).is_some() {
                return false;
            }
        }

        true
    }
}

#[derive(Debug, Clone, Default)]
pub struct BlockType {
    pub category:       String,
    pub name:           String,
    pub rows:           usize,
    pub inputs:         Vec<Option<String>>,
    pub outputs:        Vec<Option<String>>,
    pub area_count:     usize,
    pub user_input:     bool,
    pub description:    String,
}

impl BlockType {
    fn instanciate_block(&self, user_input: Option<String>) -> Box<Block> {
        Box::new(Block {
            rows:     self.rows,
            contains:
                match self.area_count {
                    0 => (None, None),
                    1 => (Some(1), None),
                    2 => (Some(1), Some(1)),
                    _ => (None, None),
                },
            expanded: true,
            typ:      self.name.clone(),
            lbl:
                if let Some(inp) = user_input { inp }
                else { self.name.clone() },
            inputs:   self.inputs.clone(),
            outputs:  self.outputs.clone(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct BlockLanguage {
    types:  HashMap<String, BlockType>,
}

impl BlockLanguage {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    pub fn define(&mut self, typ: BlockType) {
        self.types.insert(typ.name.clone(), typ);
    }

    pub fn get_type_list(&self) -> Vec<(String, String, bool)> {
        let mut out = vec![];
        for (_, typ) in &self.types {
            out.push((typ.category.clone(), typ.name.clone(), typ.user_input));
        }
        out
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockDSPError {
    UnknownArea(usize),
    UnknownLanguageType(String),
    NoSpaceAvailable(usize, usize, usize),
}

#[derive(Debug, Clone)]
pub struct BlockCode {
    language:   Rc<RefCell<BlockLanguage>>,
    areas:      Vec<BlockArea>,
}

impl BlockCode {
    pub fn new(lang: Rc<RefCell<BlockLanguage>>) -> Self {
        Self {
            language: lang,
            areas:    vec![BlockArea::new(16, 16)],
        }
    }

    pub fn instanciate_at(
        &mut self, id: usize, x: usize, y: usize,
        typ: &str, user_input: Option<String>
    ) -> Result<(), BlockDSPError>
    {
        let lang = self.language.borrow();

        println!("TYPES: {:?}", lang.types);

        if let Some(area) = self.areas.get_mut(id) {
            if let Some(typ) = lang.types.get(typ) {
                if !area.check_space_at(x, y, typ.rows) {
                    return Err(BlockDSPError::NoSpaceAvailable(x, y, typ.rows));
                }
            }
        } else {
            return Err(BlockDSPError::UnknownArea(id));
        }

        if let Some(typ) = lang.types.get(typ) {
            let mut block = typ.instanciate_block(user_input);

            if let Some(area_id) = &mut block.contains.0 {
                self.areas.push(BlockArea::new(1, 1));
                *area_id = self.areas.len() - 1;
            }

            if let Some(area_id) = &mut block.contains.1 {
                self.areas.push(BlockArea::new(1, 1));
                *area_id = self.areas.len() - 1;
            }

            if let Some(area) = self.areas.get_mut(id) {
                area.set_block_at(x, y, block);
            }

            Ok(())
        } else {
            return Err(BlockDSPError::UnknownLanguageType(typ.to_string()));
        }
    }

    pub fn area_size(&self, id: usize) -> (usize, usize) {
        self.areas.get(id).map(|a| a.size).unwrap_or((0, 0))
    }

    pub fn block_at(&self, id: usize, x: usize, y: usize) -> Option<&dyn BlockView> {
        let area  = self.areas.get(id)?;
        Some(area.blocks.get(&(x, y))?.as_ref())
    }

    pub fn origin_at(&self, id: usize, x: usize, y: usize) -> Option<(usize, usize)> {
        self.areas
            .get(id)
            .map(|a| a.origin_map.get(&(x, y)).copied())
            .flatten()
    }
}

impl BlockCodeView for BlockCode {
    fn area_size(&self, id: usize) -> (usize, usize) {
        self.area_size(id)
    }

    fn block_at(&self, id: usize, x: usize, y: usize) -> Option<&dyn BlockView> {
        self.block_at(id, x, y)
    }

    fn origin_at(&self, id: usize, x: usize, y: usize)
        -> Option<(usize, usize)>
    {
        self.origin_at(id, x, y)
    }
}

