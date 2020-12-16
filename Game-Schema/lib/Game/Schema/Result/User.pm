package Game::Schema::Result::User;

use base qw(Game::Schema::Result);

__PACKAGE__->table("users");
__PACKAGE__->add_columns(qw(uuid email password salt status created_at));
__PACKAGE__->set_primary_key("uuid");
__PACKAGE__->has_many(players => "Game::Schema::Result::Player", "user_id");

1;
