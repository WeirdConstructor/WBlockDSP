use crate::block_view::{BlockView, BlockCodeView};
use std::rc::Rc;
use std::cell::RefCell;

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

/// This structure represents a block inside the [BlockArea] of a [BlockFun].
/// It stores everything required for calculating a node of the AST.
///
/// A [BlockType::instanciate_block] is used to create a new instance of this
/// structure.
///
/// You usually don't use this structure directly, but you use the
/// position of it inside the [BlockFun]. The position of a block
/// is specified by the `area_id`, and the `x` and `y` coordinates.
#[derive(Debug, Clone)]
pub struct Block {
    /// How many rows this block spans. A [Block] can only be 1 cell wide.
    rows:     usize,
    /// Up to two sub [BlockArea] can be specified here by their ID.
    contains: (Option<usize>, Option<usize>),
    /// Whether the sub areas are visible/drawn.
    expanded: bool,
    /// The type of this block. It's just a string set by the [BlockType]
    /// and it should be everything that determines what this block is
    /// going to end up as in the AST.
    typ:      String,
    /// The label of the block.
    lbl:      String,
    /// The input ports, the index into the [Vec] is the row. The [String]
    /// is the label of the input port.
    inputs:   Vec<Option<String>>,
    /// The output ports, the index into the [Vec] is the row. The [String]
    /// is the label of the output port.
    outputs:  Vec<Option<String>>,
    /// The color index of this block.
    color:    usize,
}

impl Block {
    /// Takes the (input) port at row `idx` and pushed it one row further
    /// down, wrapping around at the end. If `output` is true, the
    /// output port at `idx` is shifted.
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

    /// Calls `f` for every output port that is available.
    /// `f` gets passed the row index.
    pub fn for_output_ports<F: FnMut(usize)>(&self, mut f: F) {
        for i in 0..self.rows {
            if let Some(o) = self.outputs.get(i) {
                if o.is_some() {
                    f(i);
                }
            }
        }
    }

    /// Calls `f` for every input port that is available.
    /// `f` gets passed the row index.
    pub fn for_input_ports<F: FnMut(usize)>(&self, mut f: F) {
        for i in 0..self.rows {
            if let Some(o) = self.inputs.get(i) {
                if o.is_some() {
                    f(i);
                }
            }
        }
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
    fn custom_color(&self) -> Option<usize> { Some(self.color) }
}

/// Represents a connected collection of blocks. Is created by
/// [BlockFun::retrieve_block_chain_at] or [BlockArea::chain_at].
///
/// After creating a [BlockChain] structure you can decide to
/// clone the blocks from the [BlockArea] with [BlockChain::clone_load]
/// or remove the blocks from the [BlockArea] and store them
/// inside this [BlockChain] via [BlockChain::remove_load].
///
/// The original positions of the _loaded_ blocks is stored too.
/// If you want to move the whole chain in the coordinate system
/// to the upper left most corner, you can use [BlockChain::normalize_load_pos].
#[derive(Debug, Clone)]
pub struct BlockChain {
    /// The area ID this BlockChain was created from.
    area_id: usize,
    /// Stores the positions of the blocks of the chain inside the [BlockArea].
    blocks:  HashSet<(i64, i64)>,
    /// Stores the positions of blocks that only have output ports.
    sources: HashSet<(i64, i64)>,
    /// Stores the positions of blocks that only have input ports.
    sinks:   HashSet<(i64, i64)>,
    /// This field stores _loaded_ blocks from the [BlockArea]
    /// into this [BlockChain] for inserting or analyzing them.
    ///
    /// Stores the blocks themself, with their position in the [BlockArea],
    /// which can be normalized (moved to the upper left) with
    /// [BlockChain::normalize_load_pos].
    ///
    /// The blocks in this [Vec] are stored in sorted order.
    /// They are stored in ascending order of their `x` coordinate,
    /// and for the same `x` coordinate in
    /// ascending order of their `y` coordinate.
    load:    Vec<(Box<Block>, i64, i64)>,
}

impl BlockChain {
    pub fn move_by_offs(&mut self, xo: i64, yo: i64) {
        for (_, x, y) in &mut self.load {
            *x += xo;
            *y += yo;
        }
    }

    /// Normalizes the position of all loaded blocks and returns
    /// the original top left most position of the chain.
    pub fn normalize_load_pos(&mut self) -> (i64, i64) {
        let mut min_x = 100000000;
        let mut min_y = 100000000;

        for (_, xo, yo) in &self.load {
            min_x = min_x.min(*xo);
            min_y = min_y.min(*yo);
        }

        for (_, xo, yo) in &mut self.load {
            *xo -= min_x;
            *yo -= min_y;
        }

        self.sort_load_pos();

        (min_x, min_y)
    }

    fn sort_load_pos(&mut self) {
        self.load.sort_by(|&(_, x0, y0), &(_, x1, y1)| {
            x0.cmp(&x1).then(y0.cmp(&y1))
        });
    }

    pub fn clone_load(&mut self, area: &mut BlockArea) {
        self.load.clear();

        for b in &self.blocks {
            if let Some((block, xo, yo)) = area.ref_at_origin(b.0, b.1) {
                self.load.push((Box::new(block.clone()), xo, yo));
            }
        }

        self.sort_load_pos();
    }

    pub fn remove_load(&mut self, area: &mut BlockArea) {
        self.load.clear();

        for b in &self.blocks {
            if let Some((block, xo, yo)) = area.remove_at(b.0, b.1) {
                self.load.push((block, xo, yo));
            }
        }

        self.sort_load_pos();
    }
}

#[derive(Debug, Clone)]
pub struct BlockArea {
    blocks:      HashMap<(i64, i64), Box<Block>>,
    origin_map:  HashMap<(i64, i64), (i64, i64)>,
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

    pub fn chain_at(&self, x: i64, y: i64) -> Option<Box<BlockChain>> {
        let (block, xo, yo) = self.ref_at_origin(x, y)?;

        let mut dq : VecDeque<(i64, i64)> = VecDeque::new();
        dq.push_back((xo, yo));

        let mut blocks  : HashSet<(i64, i64)> = HashSet::new();
        let mut sources : HashSet<(i64, i64)> = HashSet::new();
        let mut sinks   : HashSet<(i64, i64)> = HashSet::new();

        let mut check_port_conns = vec![];

        while let Some((x, y)) = dq.pop_front() {
            check_port_conns.clear();

            // First we find all adjacent output/input port positions
            // and collect them in `check_port_conns`.
            //
            // While are at it, we also record which blocks are only
            // sinks and which are only sources. Might be important for
            // other algorithms that do things with this.
            if let Some((block, xo, yo)) = self.ref_at_origin(x, y) {
                if blocks.contains(&(xo, yo)) {
                    continue;
                }

                blocks.insert((xo, yo));

                let mut has_input  = false;
                let mut has_output = false;

                if xo > 0 {
                    block.for_input_ports(|idx| {
                        check_port_conns.push((xo - 1, yo + (idx as i64), true));
                        has_input = true;
                    });
                }

                block.for_output_ports(|idx| {
                    check_port_conns.push((xo + 1, yo + (idx as i64), false));
                    has_output = true;
                });

                if !has_input {
                    sources.insert((xo, yo));
                }

                if !has_output {
                    sinks.insert((xo, yo));
                }
            }

            // Then we look if there is a block at that position, with
            // a corresponding input or output port at the right
            // row inside the block.
            for (x, y, is_output) in &check_port_conns {
                if let Some((block, xo, yo)) = self.ref_at_origin(*x, *y) {
                    let port_y = (y - yo).max(0) as usize;

                    if *is_output {
                        if let Some(o) = block.outputs.get(port_y) {
                            if o.is_some() { dq.push_back((xo, yo)); }
                        }
                    } else {
                        if let Some(i) = block.inputs.get(port_y) {
                            if i.is_some() { dq.push_back((xo, yo)); }
                        }
                    }
                }
            }
        }

        Some(Box::new(BlockChain {
            area_id: 0,
            blocks,
            sources,
            sinks,
            load: vec![],
        }))
    }

    fn ref_at(&self, x: i64, y: i64) -> Option<&Block> {
        let (xo, yo) = self.origin_map.get(&(x, y))?;
        self.blocks.get(&(*xo, *yo)).map(|b| b.as_ref())
    }

    fn ref_at_origin(&self, x: i64, y: i64) -> Option<(&Block, i64, i64)> {
        let (xo, yo) = self.origin_map.get(&(x, y))?;
        let (xo, yo) = (*xo, *yo);
        self.blocks.get(&(xo, yo)).map(|b| (b.as_ref(), xo, yo))
    }

    fn ref_mut_at(&mut self, x: i64, y: i64) -> Option<&mut Block> {
        let (xo, yo) = self.origin_map.get(&(x, y))?;
        self.blocks.get_mut(&(*xo, *yo)).map(|b| b.as_mut())
    }

    fn ref_mut_at_origin(&mut self, x: i64, y: i64) -> Option<(&mut Block, i64, i64)> {
        let (xo, yo) = self.origin_map.get(&(x, y))?;
        let (xo, yo) = (*xo, *yo);
        self.blocks.get_mut(&(xo, yo)).map(|b| (b.as_mut(), xo, yo))
    }

    fn set_block_at(&mut self, x: i64, y: i64, block: Box<Block>) {
        self.blocks.insert((x, y), block);
        self.update_origin_map();
    }

    fn remove_at(&mut self, x: i64, y: i64) -> Option<(Box<Block>, i64, i64)> {
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

    /// Calculates only the size of the area in the +x/+y quadrant.
    /// The negative areas are not counted in.
    fn resolve_size<F: Fn(usize) -> (usize, usize)>(
        &self, resolve_sub_areas: F
    ) -> (usize, usize)
    {
        let mut min_w = 1;
        let mut min_h = 1;

        for ((ox, oy), _) in &self.origin_map {
            let (ox, oy) = ((*ox).max(0) as usize, (*oy).max(0) as usize);

            if min_w < (ox + 1) { min_w = ox + 1; }
            if min_h < (oy + 1) { min_h = oy + 1; }
        }

        for ((x, y), block) in &self.blocks {
            let (x, y) = ((*x).max(0) as usize, (*y).max(0) as usize);

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
                self.origin_map.insert((*ox, *oy + (r as i64)), (*ox, *oy));
            }
        }
    }

    fn check_space_at(&self, x: i64, y: i64, rows: usize) -> bool {
        for i in 0..rows {
            let yo = y + (i as i64);

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
    pub color:          usize,
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

    pub fn instanciate_block(&self, user_input: Option<String>) -> Box<Block> {
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
            color:    self.color,
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
    NoBlockAt(usize, i64, i64),
    CircularAction(usize, usize),
    NoSpaceAvailable(usize, i64, i64, usize),
}

#[derive(Debug, Clone)]
pub struct BlockFun {
    language:       Rc<RefCell<BlockLanguage>>,
    areas:          Vec<Box<BlockArea>>,
    size_work_dq:   VecDeque<usize>,
    area_work_dq:   VecDeque<usize>,
}

impl BlockFun {
    pub fn new(lang: Rc<RefCell<BlockLanguage>>) -> Self {
        Self {
            language:     lang,
            areas:        vec![Box::new(BlockArea::new(16, 16))],
            size_work_dq: VecDeque::new(),
            area_work_dq: VecDeque::new(),
        }
    }

    pub fn block_ref(
        &self, id: usize, x: i64, y: i64
    ) -> Option<&Block> {
        let area = self.areas.get(id)?;
        area.ref_at(x, y)
    }

    pub fn block_ref_mut(
        &mut self, id: usize, x: i64, y: i64
    ) -> Option<&mut Block> {
        let area = self.areas.get_mut(id)?;
        area.ref_mut_at(x, y)
    }

    pub fn shift_port(
        &mut self, id: usize, x: i64, y: i64,
        row: usize,
        output: bool)
    {
        if let Some(block) = self.block_ref_mut(id, x, y) {
            block.shift_port(row, output);
        }
    }

    pub fn recalculate_area_sizes(&mut self) {
        let mut parents = vec![0;      self.areas.len()];
        let mut sizes   = vec![(0, 0); self.areas.len()];

        // First we dive downwards, to record all the parents
        // and get the sizes of the (leafs).

        self.area_work_dq.clear();
        self.size_work_dq.clear();

        let mut parents_work_list = &mut self.area_work_dq;
        let mut size_work_list    = &mut self.size_work_dq;

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

            // XXX: area_idx == 0 is the root area, so skip that
            //      when pushing further parents!
            if area_idx > 0 {
                size_work_list.push_back(parents[area_idx]);
            }
        }
    }

    pub fn area_is_subarea_of(
        &mut self, area_id: usize, blocks: &[(usize, i64, i64)]
    ) -> bool
    {
        let mut areas = vec![];

        for (a_id, x, y) in blocks {
            self.all_sub_areas_of(*a_id, *x, *y, &mut areas);
        }

        for a_id in &areas {
            if area_id == *a_id {
                return true;
            }
        }

        return false;
    }

    pub fn all_sub_areas_of(
        &mut self, id: usize, x: i64, y: i64, areas: &mut Vec<usize>
    ) {
        let contains =
            if let Some(block) = self.block_ref(id, x, y) {
                block.contains.clone()
            } else {
                return;
            };

        let mut area_work_list = &mut self.area_work_dq;
        area_work_list.clear();

        if let Some(area_id) = contains.0 {
            area_work_list.push_back(area_id);
        }
        if let Some(area_id) = contains.1 {
            area_work_list.push_back(area_id);
        }

        if area_work_list.len() <= 0 {
            return;
        }

        let mut cur_sub = vec![];
        while let Some(area_idx) = area_work_list.pop_front() {
            areas.push(area_idx);

            cur_sub.clear();
            self.areas[area_idx].get_direct_sub_areas(&mut cur_sub);

            for sub_idx in &cur_sub {
                area_work_list.push_back(*sub_idx);
            }
        }
    }

    pub fn retrieve_block_chain_at(
        &mut self, id: usize, x: i64, y: i64, remove_blocks: bool
    ) -> Option<Box<BlockChain>> {
        let area      = self.areas.get_mut(id)?;
        let mut chain = area.chain_at(x, y)?;

        if remove_blocks {
            chain.remove_load(area);
        } else {
            chain.clone_load(area);
        }

        Some(chain)
    }

    pub fn clone_block_from_to(
        &mut self,
        id: usize, x: i64, y: i64,
        id2: usize, x2: i64, mut y2: i64
    ) -> Result<(), BlockDSPError>
    {
        let lang = self.language.clone();

        let (mut block, xo, yo) =
            if let Some(area) = self.areas.get_mut(id) {
                let (block, xo, yo) =
                    area.ref_mut_at_origin(x, y)
                        .ok_or(BlockDSPError::NoBlockAt(id, x, y))?;

                let mut new_block = Box::new(block.clone());
                if let Some(typ) = lang.borrow().types.get(&new_block.typ) {
                    typ.touch_contains(new_block.as_mut());
                }

                (new_block, xo, yo)
            } else {
                return Err(BlockDSPError::UnknownArea(id));
            };

        self.create_areas_for_block(block.as_mut());

        // check if the user grabbed at a different row than the top row:
        if y > yo {
            // if so, adjust the destination:
            let offs = y - yo;
            y2 = (y2 - offs).max(0);
        }

        let area2 =
            self.areas
                .get_mut(id2)
                .ok_or(BlockDSPError::UnknownArea(id2))?;
        let rows = block.rows;

        if area2.check_space_at(x2, y2, block.rows) {
            area2.set_block_at(x2, y2, block);
            Ok(())
        } else {
            Err(BlockDSPError::NoSpaceAvailable(id2, x2, y2, rows))
        }
    }

    pub fn move_block_chain_from_to(
        &mut self,
        id: usize, x: i64, y: i64,
        id2: usize, x2: i64, mut y2: i64
    ) -> Result<(), BlockDSPError>
    {
        let mut area_clone =
            self.areas
                .get(id)
                .ok_or(BlockDSPError::UnknownArea(id))?
                .clone();

        let mut chain =
            area_clone.chain_at(x, y)
                .ok_or(BlockDSPError::NoBlockAt(id, x, y))?;

        chain.remove_load(area_clone.as_mut());
        let (xo, yo) = chain.normalize_load_pos();

        // From the top/left position of the original chain,
        // calculate the offset to where we grabbed it.
        let (grab_x_offs, grab_y_offs) = (x - xo, y - yo);

//        let mut chain =
//            self.retrieve_block_chain_at(id, x, y, true)
//                .ok_or(BlockDSPError::NoBlockAt(id, x, y))?;
//
//        chain.normalize_load_pos();

        Ok(())
    }

    pub fn move_block_from_to(
        &mut self,
        id: usize, x: i64, y: i64,
        id2: usize, x2: i64, mut y2: i64
    ) -> Result<(), BlockDSPError>
    {
        // TODO: We must prevent a block from being moved to any of their
        //       sub areas! For this we need to dive into the current block
        //       and record if the destination is any of the sub areas!

        if self.area_is_subarea_of(id2, &[(id, x, y)]) {
            return Err(BlockDSPError::CircularAction(id, id2));
        }

        let (block, xo, yo) =
            if let Some(area) = self.areas.get_mut(id) {
                area.remove_at(x, y)
                    .ok_or(BlockDSPError::NoBlockAt(id, x, y))?
            } else {
                return Err(BlockDSPError::UnknownArea(id));
            };

        let mut res = Ok(());

        // check if the user grabbed at a different row than the top row:
        if y > yo {
            // if so, adjust the destination:
            let offs = y - yo;
            y2 = (y2 - offs).max(0);
        }

        let area2 =
            self.areas.get_mut(id2)
                .ok_or(BlockDSPError::UnknownArea(id2))?;
        let rows = block.rows;

        if area2.check_space_at(x2, y2, block.rows) {
            area2.set_block_at(x2, y2, block);
            return Ok(());
        } else {
            res = Err(BlockDSPError::NoSpaceAvailable(id2, x2, y2, rows));
        }

        if let Err(e) = res {
            if let Some(area) = self.areas.get_mut(id) {
                area.set_block_at(xo, yo, block);
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
        &mut self, id: usize, x: i64, y: i64,
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

            let typ =
                lang.types.get(typ)
                    .ok_or(
                        BlockDSPError::UnknownLanguageType(
                            typ.to_string()))?;
            typ.instanciate_block(user_input)
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

    pub fn block_at(&self, id: usize, x: i64, y: i64) -> Option<&dyn BlockView> {
        let area  = self.areas.get(id)?;
        Some(area.blocks.get(&(x, y))?.as_ref())
    }

    pub fn origin_at(&self, id: usize, x: i64, y: i64) -> Option<(i64, i64)> {
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

    fn block_at(&self, id: usize, x: i64, y: i64) -> Option<&dyn BlockView> {
        self.block_at(id, x, y)
    }

    fn origin_at(&self, id: usize, x: i64, y: i64)
        -> Option<(i64, i64)>
    {
        self.origin_at(id, x, y)
    }
}

