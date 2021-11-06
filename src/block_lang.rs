use crate::block_view::{BlockView, BlockCodeView};
use std::rc::Rc;
use std::cell::RefCell;

use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct Block {
    rows:     usize,
    contains: (Option<usize>, Option<usize>),
    expanded: bool,
    typ:      String,
    lbl:      String,
    inputs:   Vec<Option<String>>,
    outputs:  Vec<Option<String>>,
}

impl Clone for Block {
    fn clone(&self) -> Self {
        Self {
            rows:     self.rows,
            contains: (None, None),
            expanded: self.expanded,
            typ:      self.typ.clone(),
            lbl:      self.lbl.clone(),
            inputs:   self.inputs.clone(),
            outputs:  self.outputs.clone(),
        }
    }
}

impl Block {
    pub fn shift_port(&mut self, idx: usize, output: bool) {
        if self.rows <= 1 {
            return;
        }

        let v =
            if output { &mut self.outputs }
            else      { &mut self.inputs };

        if v.len() < self.rows {
            v.resize(self.rows, None);
        }

        let idx_from = idx;
        let idx_to   = (idx + 1) % v.len();
        println!("{} => {} [{}]", idx_from, idx_to, v.len());
        let elem = v.remove(idx_from);
        v.insert(idx_to, elem);
    }
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
    blocks:      HashMap<(usize, usize), Box<Block>>,
    origin_map:  HashMap<(usize, usize), (usize, usize)>,
    size:        (usize, usize),
    auto_shrink: bool,
}

impl BlockArea {
    fn new(w: usize, h: usize) -> Self {
        Self {
            blocks:      HashMap::new(),
            origin_map:  HashMap::new(),
            size:        (w, h),
            auto_shrink: false,
        }
    }

    pub fn set_auto_shrink(&mut self, shrink: bool) {
        self.auto_shrink = shrink;
    }

    pub fn auto_shrink(&self) -> bool { self.auto_shrink }

    fn ref_mut_at(&mut self, x: usize, y: usize) -> Option<&mut Block> {
        let (xo, yo) = self.origin_map.get(&(x, y))?;
        self.blocks.get_mut(&(*xo, *yo)).map(|b| b.as_mut())
    }

    fn ref_mut_at_origin(&mut self, x: usize, y: usize) -> Option<(&mut Block, usize, usize)> {
        let (xo, yo) = self.origin_map.get(&(x, y))?;
        let (xo, yo) = (*xo, *yo);
        self.blocks.get_mut(&(xo, yo)).map(|b| (b.as_mut(), xo, yo))
    }

    fn set_block_at(&mut self, x: usize, y: usize, block: Box<Block>) {
        self.blocks.insert((x, y), block);
        self.update_origin_map();
    }

    fn remove_block_at(&mut self, x: usize, y: usize) -> Option<(Box<Block>, usize, usize)> {
        let (xo, yo) = self.origin_map.get(&(x, y))?;
        if let Some(block) = self.blocks.remove(&(*xo, *yo)) {
            let (xo, yo) = (*xo, *yo);
            self.update_origin_map();
            Some((block, xo, yo))
        } else {
            None
        }
    }

    fn set_size(&mut self, w: usize, h: usize) {
        self.size = (w, h);
    }

    fn get_direct_sub_areas(&self, out: &mut Vec<usize>) {
        for ((x, y), block) in &self.blocks {
            if let Some(sub_area) = block.contains.0 {
                out.push(sub_area);
            }

            if let Some(sub_area) = block.contains.1 {
                out.push(sub_area);
            }
        }
    }

    fn resolve_size<F: Fn(usize) -> (usize, usize)>(
        &self, resolve_sub_areas: F
    ) -> (usize, usize)
    {
        let mut min_w = 1;
        let mut min_h = 1;

        for ((ox, oy), _) in &self.origin_map {
            if min_w < (ox + 1) { min_w = ox + 1; }
            if min_h < (oy + 1) { min_h = oy + 1; }
        }

        for ((x, y), block) in &self.blocks {
            let mut prev_h = 1; // one for the top block

            if let Some(sub_area) = block.contains.0 {
                let (sub_w, mut sub_h) = resolve_sub_areas(sub_area);
                sub_h += prev_h;
                prev_h += sub_h;
                if min_w < (x + sub_w + 1) { min_w = x + sub_w + 1; }
                if min_h < (y + sub_h + 1) { min_h = y + sub_h + 1; }
            }

            if let Some(sub_area) = block.contains.1 {
                let (sub_w, mut sub_h) = resolve_sub_areas(sub_area);
                sub_h += prev_h;
                if min_w < (x + sub_w + 1) { min_w = x + sub_w + 1; }
                if min_h < (y + sub_h + 1) { min_h = y + sub_h + 1; }
            }
        }

        if self.auto_shrink {
            (min_w, min_h)

        } else {
            (
                if self.size.0 < min_w { min_w } else { self.size.0 },
                if self.size.1 < min_h { min_h } else { self.size.1 },
            )
        }
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
    fn touch_contains(&self, block: &mut Block) {
        block.contains =
            match self.area_count {
                0 => (None, None),
                1 => (Some(1), None),
                2 => (Some(1), Some(1)),
                _ => (None, None),
            };
    }

    fn instanciate_block(&self, user_input: Option<String>) -> Box<Block> {
        let mut block = Box::new(Block {
            rows:     self.rows,
            contains: (None, None),
            expanded: true,
            typ:      self.name.clone(),
            lbl:
                if let Some(inp) = user_input { inp }
                else { self.name.clone() },
            inputs:   self.inputs.clone(),
            outputs:  self.outputs.clone(),
        });
        self.touch_contains(&mut *block);
        block
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
    NoBlockAt(usize, usize, usize),
    MoveOutside(usize, usize, usize),
    NoSpaceAvailable(usize, usize, usize, usize),
}

#[derive(Debug, Clone)]
pub struct BlockFun {
    language:   Rc<RefCell<BlockLanguage>>,
    areas:      Vec<Box<BlockArea>>,
}

impl BlockFun {
    pub fn new(lang: Rc<RefCell<BlockLanguage>>) -> Self {
        Self {
            language: lang,
            areas:    vec![Box::new(BlockArea::new(16, 16))],
        }
    }

    pub fn block_ref_mut(
        &mut self, id: usize, x: usize, y: usize
    ) -> Option<&mut Block> {
        let area = self.areas.get_mut(id)?;
        area.ref_mut_at(x, y)
    }

    pub fn shift_port(
        &mut self, id: usize, x: usize, y: usize,
        row: usize,
        output: bool)
    {
        if let Some(block) = self.block_ref_mut(id, x, y) {
            block.shift_port(row, output);
        }
    }

    pub fn recalculate_area_sizes(&mut self) {
        // XXX: this algorithm should work correctly,
        // because we are always walking upwards!
        // - calculate area tree, note current area size vector
        // - push the leafs onto a stack
        // - take area from stack
        //   - recalculate size, update current area size vector
        //   - push parent areas onto stack.

        let mut parents = vec![0;      self.areas.len()];
        let mut sizes   = vec![(0, 0); self.areas.len()];

        // First we dive downwards, to record all the parents
        // and get the sizes of the (leafs).

        let mut parents_work_list : VecDeque<usize> = VecDeque::new();
        let mut size_work_list    : VecDeque<usize> = VecDeque::new();

        parents_work_list.push_back(0);

        let mut cur_sub = vec![];
        while let Some(area_idx) = parents_work_list.pop_back() {
            cur_sub.clear();

            self.areas[area_idx].get_direct_sub_areas(&mut cur_sub);

            // XXX: The resolver gets (0, 0), thats wrong for the
            //      areas with sub areas. But it resolves the leaf area
            //      sizes already correctly!
            let (w, h) = self.areas[area_idx].resolve_size(|_id| (0, 0));
            sizes[area_idx] = (w, h);
            println!("AREA ID {} = {},{}", area_idx, w, h);

            if cur_sub.len() == 0 {
                size_work_list.push_front(area_idx);

            } else {
                for sub_idx in &cur_sub {
                    // XXX: Record the parent:
                    parents[*sub_idx] = area_idx;
                    parents_work_list.push_back(*sub_idx);
                }
            }
        }

        // XXX: Invariant now is:
        //      - `parents` contains all the parent area IDs.
        //      - `size_work_list` contains all the leaf area IDs.
        //      - `sizes`   contains correct sizes for the leafs
        //                  (but wrong for the non leafs).

        // Next we need to work through the size_work_list upwards.
        // That means, for each leaf in front of the Deque,
        // we push the parent to the back.
        while let Some(area_idx) = size_work_list.pop_front() {
            // XXX: The invariant as we walk upwards is, that once we
            //      encounter a parent area ID in the size_work_list,
            //      we know that all sub areas already have been computed.
            let (w, h) = self.areas[area_idx].resolve_size(|id| sizes[id]);
            sizes[area_idx] = (w, h);
            self.areas[area_idx].set_size(w, h);
            println!("AREA ID {} == {},{}", area_idx, w, h);

            // XXX: area_idx == 0 is the root area, so skip that
            //      when pushing further parents!
            if area_idx > 0 {
                size_work_list.push_back(parents[area_idx]);
            }
        }
    }

    pub fn clone_block_from_to(
        &mut self,
            id: usize, x: usize, y: usize,
            id2: usize, x2: usize, mut y2: usize
    ) -> Result<(), BlockDSPError>
    {
        let lang = self.language.clone();

        let (mut block, xo, yo) =
            if let Some(area) = self.areas.get_mut(id) {
                if let Some((block, xo, yo)) = area.ref_mut_at_origin(x, y) {
                    let mut new_block = Box::new(block.clone());
                    if let Some(typ) = lang.borrow().types.get(&new_block.typ) {
                        typ.touch_contains(new_block.as_mut());
                    }

                    (new_block, xo, yo)
                } else {
                    return Err(BlockDSPError::NoBlockAt(id, x, y));
                }
            } else {
                return Err(BlockDSPError::UnknownArea(id));
            };

        self.create_areas_for_block(block.as_mut());

        // check if the user grabbed at a different row than the top row:
        if y > yo {
            // if so, adjust the destination:
            let offs = y - yo;
            if offs > y2 {
                return Err(BlockDSPError::MoveOutside(id2, x2, y2));
            } else {
                y2 = y2 - offs;
            }
        }

        if let Some(area2) = self.areas.get_mut(id2) {
            let rows = block.rows;

            if area2.check_space_at(x2, y2, block.rows) {
                area2.set_block_at(x2, y2, block);
                Ok(())
            } else {
                Err(BlockDSPError::NoSpaceAvailable(id2, x2, y2, rows))
            }
        } else {
            Err(BlockDSPError::UnknownArea(id2))
        }
    }

    pub fn move_block_from_to(
        &mut self,
            id: usize, x: usize, y: usize,
            id2: usize, x2: usize, mut y2: usize
    ) -> Result<(), BlockDSPError>
    {
        // TODO: We must prevent a block from being moved to any of their
        //       sub areas! For this we need to dive into the current block
        //       and record if the destination is any of the sub areas!

        let (block, xo, yo) =
            if let Some(area) = self.areas.get_mut(id) {
                if let Some((block, xo, yo)) = area.remove_block_at(x, y) {
                    (block, xo, yo)
                } else {
                    return Err(BlockDSPError::NoBlockAt(id, x, y));
                }
            } else {
                return Err(BlockDSPError::UnknownArea(id));
            };

        let mut res = Ok(());

        // check if the user grabbed at a different row than the top row:
        if y > yo {
            // if so, adjust the destination:
            let offs = y - yo;
            if offs > y2 {
                res = Err(BlockDSPError::MoveOutside(id2, x2, y2));
            } else {
                y2 = y2 - offs;
            }
        }

        if let Some(area2) = self.areas.get_mut(id2) {
            let rows = block.rows;

            if area2.check_space_at(x2, y2, block.rows) {
                area2.set_block_at(x2, y2, block);
                return Ok(());
            } else {
                res = Err(BlockDSPError::NoSpaceAvailable(id2, x2, y2, rows));
            }
        } else {
            return Err(BlockDSPError::UnknownArea(id2));
        }

        if let Err(e) = res {
            if let Some(area) = self.areas.get_mut(id) {
                area.set_block_at(x, y, block);
            }
            Err(e)
        } else {
            Ok(())
        }
    }

    fn create_areas_for_block(&mut self, block: &mut Block) {
        if let Some(area_id) = &mut block.contains.0 {
            let mut area = Box::new(BlockArea::new(1, 1));
            area.set_auto_shrink(true);
            self.areas.push(area);
            *area_id = self.areas.len() - 1;
        }

        if let Some(area_id) = &mut block.contains.1 {
            let mut area = Box::new(BlockArea::new(1, 1));
            area.set_auto_shrink(true);
            self.areas.push(area);
            *area_id = self.areas.len() - 1;
        }
    }

    pub fn instanciate_at(
        &mut self, id: usize, x: usize, y: usize,
        typ: &str, user_input: Option<String>
    ) -> Result<(), BlockDSPError>
    {
        let mut block = {
            let lang = self.language.borrow();

            if let Some(area) = self.areas.get_mut(id) {
                if let Some(typ) = lang.types.get(typ) {
                    if !area.check_space_at(x, y, typ.rows) {
                        return Err(
                            BlockDSPError::NoSpaceAvailable(id, x, y, typ.rows));
                    }
                }
            } else {
                return Err(BlockDSPError::UnknownArea(id));
            }

            if let Some(typ) = lang.types.get(typ) {
                typ.instanciate_block(user_input)
            } else {
                return Err(BlockDSPError::UnknownLanguageType(typ.to_string()));
            }
        };

        self.create_areas_for_block(block.as_mut());

        if let Some(area) = self.areas.get_mut(id) {
            area.set_block_at(x, y, block);
        }

        Ok(())
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

impl BlockCodeView for BlockFun {
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

