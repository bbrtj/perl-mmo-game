package Schema::Result::CharacterVariables;

use parent qw(Schema::Result);

use header;

__PACKAGE__->essentials("character_variables");

__PACKAGE__->belongs_to(character => "Schema::Result::Character", "id");

