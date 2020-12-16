package Game::Schema::Result::User;

use base qw(DBIx::Class::Core);

__PACKAGE__->table("users");
__PACKAGE__->add_columns(qw(id email password salt status created_at));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->has_many(players => "Game::Schema::Result::Player", "user_id");

1;
