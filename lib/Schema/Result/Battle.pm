package Schema::Result::Battle;

use parent qw(Schema::Result);

use header;

__PACKAGE__->table("battles");
__PACKAGE__->add_columns(qw(id location_id size_x size_y turn));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->has_many(contestants => "Schema::Result::BattleContestant", "battle_id");

