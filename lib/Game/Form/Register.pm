package Game::Form::Register;

use Form::Tiny -filtered, -consistent;
use DI;
use Types;

use header;

extends 'Game::Form';

with qw(Game::Form::Role::HTML);

use constant PASS_MIN_LENGTH => 8;

form_trim_strings;

# TODO: captcha field
# TODO: terms of service

form_field 'email' => (
	type => Types::SimpleStr,
	required => 1,
	data => { t => 'email', l => _tt('email address') },
);

form_field 'password' => (
	type => Types::SimpleStr,
	required => 1,
	data => { t => 'password', l => _tt('password') },
);

field_validator _tt('password must have at least [_1] characters', PASS_MIN_LENGTH)
	=> sub ($self, $value) {
		return length $value >= PASS_MIN_LENGTH;
	};

field_validator 'password must contain a digit'
	=> sub ($self, $value) {
		return $value =~ /\d/;
	};

form_field 'repeat_password' => (
	type => Types::SimpleStr,
	required => 1,
	data => { t => 'password', l => _tt('repeat password') },
);

form_cleaner sub ($self, $data) {
	$self->add_error('passwords do not match')
		unless $data->{password} eq $data->{repeat_password};

	try {
		my $user = DI->get('user_service')->find_user_by_email($data->{email});
		$self->add_error('that email is already taken');
	}
	catch ($e) {
		die $e
			unless $e isa Game::Exception::RecordDoesNotExist;
	}
};

