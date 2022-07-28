pub trait BlockView {
    fn rows(&self) -> usize;
    fn contains(&self, idx: usize) -> Option<usize>;
    fn expanded(&self) -> bool;
    fn label(&self, buf: &mut [u8]) -> usize;
    fn has_input(&self, idx: usize) -> bool;
    fn has_output(&self, idx: usize) -> bool;
    fn input_label(&self, idx: usize, buf: &mut [u8]) -> usize;
    fn output_label(&self, idx: usize, buf: &mut [u8]) -> usize;
    fn custom_color(&self) -> Option<usize>;
}

pub trait BlockCodeView {
    fn area_header(&self, id: usize) -> Option<&str>;
    fn area_size(&self, id: usize) -> (usize, usize);
    fn block_at(&self, id: usize, x: i64, y: i64) -> Option<&dyn BlockView>;
    fn origin_at(&self, id: usize, x: i64, y: i64) -> Option<(i64, i64)>;
}
