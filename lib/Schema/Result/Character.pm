package Schema::Result::Character;

use parent qw(Schema::Result);

use header;

__PACKAGE__->essentials("characters");

__PACKAGE__->belongs_to(player => "Schema::Result::Player", "player_id");
__PACKAGE__->has_one(variables => "Schema::Result::CharacterVariables", "id");

