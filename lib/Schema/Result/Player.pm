package Schema::Result::Player;

use parent qw(Schema::Result);

use header;

__PACKAGE__->table("players");
__PACKAGE__->add_columns(qw(id user_id online last_online created_at));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->belongs_to(user => "Schema::Result::User", "user_id");
__PACKAGE__->has_one(character => "Schema::Result::Character", "player_id");

