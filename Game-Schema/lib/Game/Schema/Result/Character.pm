package Game::Schema::Result::Character;

use base qw(Game::Schema::Result);

__PACKAGE__->table("characters");
__PACKAGE__->add_columns(qw(id player_id npc_id class_id name stats));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->belongs_to(player => "Game::Schema::Result::Player", "player_id");

1;
