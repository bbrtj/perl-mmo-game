package Web::Form::Login;

use Form::Tiny -filtered;
use DI;
use Types;

use header;

extends 'Web::Form';

has 'user' => (
	is => 'ro',
	writer => 'set_user',
	isa => Types::InstanceOf ['Model::User'],
	init_arg => undef,
);

form_trim_strings;

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

form_field 'remember_me' => (
	type => Types::Bool,
	default => sub { 0 },
	data => {t => 'checkbox', values => [_tt('1:[_1]', _t('remember_me'))], l => undef},
);

form_cleaner sub ($self, $data) {
	try {
		my $user = DI->get('user_service')->find_user_by_email($data->{email});
		if (!$user->verify_password($data->{password})) {
			$self->add_error('msg.invalid_credentials');
		}
		else {
			$self->set_user($user);
		}
	}
	catch ($e) {
		if ($e isa Exception::RecordDoesNotExist) {
			$self->add_error('msg.invalid_credentials');
		}
		else {
			die $e;
		}
	}
};

