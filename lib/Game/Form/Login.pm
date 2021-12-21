package Game::Form::Login;

use Form::Tiny -filtered, -consistent;
use DI;
use Types;

use header;

extends 'Game::Form';

with qw(Game::Form::Role::HTML);

has 'user' => (
	is => 'ro',
	writer => 'set_user',
	isa => Types::InstanceOf ['Game::Model::User'],
	init_arg => undef,
);

form_trim_strings;

form_field 'email' => (
	type => Types::SimpleStr,
	required => 1,
	data => { t => 'email', p => _tt('email address'), nl => 1},
);

form_field 'password' => (
	type => Types::SimpleStr,
	required => 1,
	data => { t => 'password', p => _tt('password'), nl => 1},
);

form_field 'remember_me' => (
	type => Types::Bool,
	default => sub { 0 },
	data => { t => 'checkbox', values => [_tt('1:[_1]', _tt('remember me'))], nl => 1 },
);

form_cleaner sub ($self, $data) {
	try {
		my $user = DI->get('user_service')->find_user_by_email($data->{email});
		if (!$user->verify_password($data->{password})) {
			$self->add_error('invalid email or password');
		}
		else {
			$self->set_user($user);
		}
	}
	catch ($e) {
		if ($e isa Exception::RecordDoesNotExist) {
			$self->add_error('invalid email or password');
		}
		else {
			die $e;
		}
	}
};

