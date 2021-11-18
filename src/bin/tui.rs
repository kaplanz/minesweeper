use cursive::view::Margins;
use cursive::views::*;
use cursive::{Cursive, CursiveExt};
use minesweeper::*;

fn main() {
    let mut siv = Cursive::new();

    // Add global callbacks
    siv.add_global_callback('q', |s| s.quit());
    // Create the main menu dialog
    menu(&mut siv);

    // Start the event loop
    siv.run();
}

fn menu(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::new()
            .title("Minesweeper")
            .padding(Margins::lrtb(2, 2, 1, 1))
            .content(
                LinearLayout::vertical()
                    .child(Button::new("Start", start))
                    .child(Button::new("Options", options))
                    .child(Button::new("Quit", Cursive::quit)),
            ),
    );
}

fn options(siv: &mut Cursive) {
    let mut radio = RadioGroup::<Config>::new().on_change(|s, _| {
        s.pop_layer();
    });

    siv.add_layer(
        Dialog::new().title("Options").content(
            LinearLayout::vertical()
                .child(radio.button(Config::beginner(), "Beginner"))
                .child(radio.button(Config::intermediate(), "Intermediate"))
                .child(radio.button(Config::expert(), "Expert"))
                .child(DummyView)
                .child(Button::new("Back", |s| {
                    s.pop_layer();
                })),
        ),
    );
}

fn start(_siv: &mut Cursive) {
    todo!();
}
