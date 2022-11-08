package Service::User;

use My::Moose;
use all 'Model';

use header;

has injected 'models_repo';

sub register_user ($self, $user_data)
{
	my $user = Model::User->new(
		plaintext_password => $user_data->{password},
		$user_data->%*
	);

	$self->models_repo->save($user);

	# TODO: send an email

	return $user;
}

sub find_user_by_email ($self, $email)
{
	return $self->models_repo->load(User => {email => lc $email});
}

