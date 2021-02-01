package Game::Form::Login;

use header;
use Form::Tiny -filtered, -strict;
use Game::Common::Container;
use Game::Types qw(NonEmptySimpleStr Bool);
use Form::Tiny::Error;

no header;

has 'user' => (
	is => 'ro',
	writer => 'set_user',
	isa => InstanceOf ['Game::Model::User'],
	init_arg => undef,
);

form_field 'email' => (
	type => NonEmptySimpleStr,
	required => 1,
);

form_field 'password' => (
	type => NonEmptySimpleStr,
	required => 1,
);

form_field 'remember_me' => (
	type => Bool,
	required => 1,
);

form_cleaner sub ($self, $data) {
	try {
		my $user = resolve('repo')->schema->load({email => $data->{email}});
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

1;
