package Game::Schema::Result::Player;

use base qw(DBIx::Class::Core);

__PACKAGE__->table("players");
__PACKAGE__->add_columns(qw(id user_id class_id name online created_at last_online));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->belongs_to(user => "Game::Schema::Result::User", "user_id");

1;
