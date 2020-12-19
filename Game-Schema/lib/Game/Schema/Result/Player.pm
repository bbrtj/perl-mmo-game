package Game::Schema::Result::Player;

use base qw(Game::Schema::Result);

__PACKAGE__->table("players");
__PACKAGE__->add_columns(qw(id user_id online last_online created_at));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->belongs_to(user => "Game::Schema::Result::User", "user_id");

1;
