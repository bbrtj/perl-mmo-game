package Web::Form::Register;

use My::Moose;
use My::Form::Tiny -filtered, plugins => ['+Web::HTMLFormPlugin'];

use header;

use constant PASS_MIN_LENGTH => 8;

form_trim_strings;

# TODO: captcha field
# TODO: terms of service

form_field 'email' => (
	type => SimpleStr,
	required => 1,
	data => {t => 'email', l => _t('email_address')},
);

form_field 'password' => (
	type => SimpleStr,
	required => 1,
	data => {t => 'password', l => _t('password')},
);

field_validator _t(Err::PASSWORD_TOO_SHORT, PASS_MIN_LENGTH)
	=> sub ($self, $value) {
		return length $value >= PASS_MIN_LENGTH;
	};

field_validator Err::PASSWORD_MUST_HAVE_DIGIT
	=> sub ($self, $value) {
		return $value =~ /\d/;
	};

form_field 'repeat_password' => (
	type => SimpleStr,
	required => 1,
	data => {t => 'password', l => _t('repeat_password')},
);

form_cleaner sub ($self, $data) {
	$self->add_error(Err::PASSWORDS_MISMATCH)
		unless $data->{password} eq $data->{repeat_password};

	try {
		my $user = DI->get('user_service')->find_user_by_email($data->{email});
		$self->add_error(Err::EMAIL_TAKEN);
	}
	catch ($e) {
		die $e
			unless $e isa X::RecordDoesNotExist;
	}

	# move password to plaintext_password
	$data->{plaintext_password} = delete $data->{password};
};

form_hook after_validate => sub ($self, $data) {

	# clear password fields, so that it won't end up in user's HTML (for their security)
	$self->input->{password} = '';
	$self->input->{repeat_password} = '';

	# move password
};

