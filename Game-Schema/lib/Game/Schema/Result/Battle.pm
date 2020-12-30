package Game::Schema::Result::Battle;

use base qw(Game::Schema::Result);

__PACKAGE__->table("battles");
__PACKAGE__->add_columns(qw(id location_id size_x size_y turn));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->has_many(contestants => "Game::Schema::Result::BattleContestant", "battle_id");

1;
