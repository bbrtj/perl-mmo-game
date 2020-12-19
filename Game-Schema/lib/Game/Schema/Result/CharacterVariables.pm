package Game::Schema::Result::CharacterVariables;

use base qw(Game::Schema::Result);

__PACKAGE__->table("character_variables");
__PACKAGE__->add_columns(qw(id experience location health focus));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->belongs_to(character => "Game::Schema::Result::Character", "id");

1;
