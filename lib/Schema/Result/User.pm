package Schema::Result::User;

use parent qw(Schema::Result);

use header;

__PACKAGE__->table("users");
__PACKAGE__->add_columns(qw(id email password status created_at));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->has_many(players => "Schema::Result::Player", "user_id");

