package Web::Form::Register;

use My::Moose;
use My::Form::Tiny -filtered, plugins => ['+Web::HTMLFormPlugin'];

use header;

use constant PASS_MIN_LENGTH => 8;

form_trim_strings;

# TODO: captcha field
# TODO: terms of service

form_field 'email' => (
	type => Types::SimpleStr,
	required => 1,
	data => {t => 'email', l => _t('email_address')},
);

form_field 'password' => (
	type => Types::SimpleStr,
	required => 1,
	data => {t => 'password', l => _t('password')},
);

field_validator _t('err.password_too_short[]', PASS_MIN_LENGTH)
	=> sub ($self, $value) {
		return length $value >= PASS_MIN_LENGTH;
	};

field_validator 'err.password_must_have_digit'
	=> sub ($self, $value) {
		return $value =~ /\d/;
	};

form_field 'repeat_password' => (
	type => Types::SimpleStr,
	required => 1,
	data => {t => 'password', l => _t('repeat_password')},
);

form_cleaner sub ($self, $data) {
	$self->add_error('err.passwords_mismatch')
		unless $data->{password} eq $data->{repeat_password};

	try {
		my $user = DI->get('user_service')->find_user_by_email($data->{email});
		$self->add_error('err.email_taken');
	}
	catch ($e) {
		die $e
			unless $e isa X::RecordDoesNotExist;
	}
};

