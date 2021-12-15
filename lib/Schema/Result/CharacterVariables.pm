package Schema::Result::CharacterVariables;

use parent qw(Schema::Result);

use header;

__PACKAGE__->table("character_variables");
__PACKAGE__->add_columns(qw(id experience location health mana));
__PACKAGE__->set_primary_key("id");
__PACKAGE__->belongs_to(character => "Schema::Result::Character", "id");

