use iced::{
    pure::{container, widget, Element},
    Length,
};

use crate::ui::color;

pub enum Style {
    Standard,
    Success,
    Warning,
}

impl widget::container::StyleSheet for Style {
    fn style(&self) -> widget::container::Style {
        match self {
            Self::Standard => widget::container::Style {
                border_radius: 40.0,
                background: color::BACKGROUND.into(),
                ..widget::container::Style::default()
            },
            Self::Success => widget::container::Style {
                border_radius: 40.0,
                background: color::SUCCESS_LIGHT.into(),
                text_color: color::SUCCESS.into(),
                ..widget::container::Style::default()
            },
            Self::Warning => widget::container::Style {
                border_radius: 40.0,
                background: color::WARNING_LIGHT.into(),
                text_color: color::WARNING.into(),
                ..widget::container::Style::default()
            },
        }
    }
}

pub struct Badge<S: widget::container::StyleSheet> {
    icon: widget::Text,
    style: S,
}

impl Badge<Style> {
    pub fn new(icon: widget::Text) -> Self {
        Self {
            icon,
            style: Style::Standard,
        }
    }
    pub fn style(self, style: Style) -> Self {
        Self {
            icon: self.icon,
            style,
        }
    }
}

impl<'a, Message: 'a, S: 'a + widget::container::StyleSheet> From<Badge<S>>
    for Element<'a, Message>
{
    fn from(badge: Badge<S>) -> Element<'a, Message> {
        container(badge.icon.width(Length::Units(20)))
            .width(Length::Units(40))
            .height(Length::Units(40))
            .style(badge.style)
            .center_x()
            .center_y()
            .into()
    }
}
