package Schema::Result::Player;

use parent qw(Schema::Result);

use header;

__PACKAGE__->essentials("players");

__PACKAGE__->belongs_to(user => "Schema::Result::User", "user_id");
__PACKAGE__->has_one(character => "Schema::Result::Character", "player_id");

