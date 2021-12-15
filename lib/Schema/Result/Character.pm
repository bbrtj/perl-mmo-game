package Schema::Result::Character;

use parent qw(Schema::Result);

use header;

__PACKAGE__->table("characters");
__PACKAGE__->add_columns(qw(id player_id npc_id class_id name stats));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->belongs_to(player => "Schema::Result::Player", "player_id");
__PACKAGE__->has_one(contestant => "Schema::Result::BattleContestant", "character_id", {join_type => 'left'});
__PACKAGE__->has_one(variables => "Schema::Result::CharacterVariables", "id");

