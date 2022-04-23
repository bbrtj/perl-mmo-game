package Form::CreatePlayer;

use Form::Tiny;
use DI;
use Types;

use header;

use constant NAME_MIN_LENGTH => 3;
use constant NAME_MAX_LENGTH => 20;

has 'lore_data' => (
	is => 'ro',
	default => sub { DI->get('lore_data') },
);

form_field 'name' => (
	type => Types::SimpleStr,
	required => 1,
	adjust => sub ($self, $name) { ucfirst lc $name },
);

field_validator _tph('err.name_too_short[]', NAME_MIN_LENGTH) => sub ($self, $name) {
	return length $name >= NAME_MIN_LENGTH;
};

field_validator _tph('err.name_too_long[]', NAME_MAX_LENGTH) => sub ($self, $name) {
	return length $name <= NAME_MAX_LENGTH;
};

field_validator 'err.name_must_consist_of_letters' => sub ($self, $name) {
	return $name =~ m{\A [a-z]+ \z}ix;
};

# TODO: check if name exists in database?

form_field 'class' => (
	type => Types::LoreId,
	required => 1,
	adjust => sub ($self, $class) { $self->lore_data->load($class) },
);

field_validator 'err.element_invalid' => sub ($self, $class) {
	return $self->lore_data->check($class, 'class');
};

# TODO: stats

