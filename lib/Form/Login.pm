package Form::Login;

use My::Moose;
use My::Form::Tiny;

use header;

has field 'user' => (
	writer => 1,
	isa => Types::InstanceOf ['Model::User'],
);

form_field 'email' => (
	type => Types::SimpleStr,
	required => 1,
	data => {t => 'email', p => _t('email_address'), l => undef},
);

form_field 'password' => (
	type => Types::SimpleStr,
	required => 1,
	data => {t => 'password', p => _t('password'), l => undef},
);

form_cleaner sub ($self, $data) {
	try {
		my $user = DI->get('user_service')->find_user_by_email($data->{email});
		if (!$user->verify_password($data->{password})) {
			$self->add_error('err.invalid_credentials');
		}
		else {
			$self->set_user($user);
		}
	}
	catch ($e) {
		die $e
			unless $e isa X::RecordDoesNotExist;

		$self->add_error('err.invalid_credentials');
	}
};

