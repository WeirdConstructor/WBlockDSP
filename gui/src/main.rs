#[allow(warnings)]
use tuix::*;
#[allow(warnings)]
use tuix::widgets::*;

pub fn main() {
    println!("FOO");
    let app =
        Application::new(
            WindowDescription::new().with_inner_size(900, 760),
            |state, window| {
            });
    app.run();
}
