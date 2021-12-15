package Schema::Result::BattleContestant;

use parent qw(Schema::Result);

use header;

__PACKAGE__->table("battle_contestants");
__PACKAGE__->add_columns(qw(id battle_id character_id pos_x pos_y initiative team));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->belongs_to(battle => "Schema::Result::Battle", "battle_id");
__PACKAGE__->belongs_to(character => "Schema::Result::Character", "character_id");

