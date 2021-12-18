package Game::Form::Register;

use Form::Tiny -filtered, -consistent;
use DI;
use Types;

use header;

extends 'Game::Form';

my $pass_min_len = 8;

form_trim_strings;

form_field 'email' => (
	type => Types::SimpleStr,
	required => 1,
);

form_field 'password' => (
	type => Types::SimpleStr,
	required => 1,
);

field_validator _tt('password must have at least [_1] characters', $pass_min_len)
	=> sub ($self, $value) {
		return length $value >= $pass_min_len;
	};

field_validator 'password must contain a digit'
	=> sub ($self, $value) {
		return $value =~ /\d/;
	};

form_field 'repeat_password' => (
	type => Types::SimpleStr,
	required => 1,
);

form_cleaner sub ($self, $data) {
	$self->add_error('passwords do not match')
		unless $data->{password} eq $data->{repeat_password};
};

