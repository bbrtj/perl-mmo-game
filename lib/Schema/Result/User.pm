package Schema::Result::User;

use parent qw(Schema::Result);

use header;

__PACKAGE__->essentials("users");

__PACKAGE__->has_many(players => "Schema::Result::Player", "user_id");

