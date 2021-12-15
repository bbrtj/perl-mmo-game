package Game::Form::Login;

use Form::Tiny -filtered, -consistent;
use DI;
use Types;
use Form::Tiny::Error;

use header;

has 'user' => (
	is => 'ro',
	writer => 'set_user',
	isa => Types::InstanceOf ['Game::Model::User'],
	init_arg => undef,
);

form_field 'email' => (
	type => Types::NonEmptySimpleStr,
	required => 1,
);

form_field 'password' => (
	type => Types::NonEmptySimpleStr,
	required => 1,
);

form_field 'remember_me' => (
	type => Types::Bool,
	required => 1,
);

form_cleaner sub ($self, $data) {
	try {
		my $user = DI->get('repo')->schema->load({email => $data->{email}});
		if (!$user->verify_password($data->{password})) {
			$self->add_error(
				Form::Tiny::Error::DoesNotValidate->new(
					field => 'password',
					error => 'invalid password'
				)
			);
		}
		else {
			$self->set_user($user);
		}
	}
	catch ($e) {
		if ($e->$_isa(Game::Exception::RecordDoesNotExist::)) {
			$self->add_error(
				Form::Tiny::Error::DoesNotValidate->new(
					field => 'email',
					error => 'invalid email address'
				)
			);
		}
		else {
			$self->add_error(
				Form::Tiny::Error->new(
					error => 'unknown error'
				)
			);
		}
	}
};

