package Service::User;

use My::Moose;
use Model;

use header;

has 'repo' => (
	is => 'ro',
);

sub register_user ($self, $user_data)
{
	my $user = Model::User->new($user_data);
	$user->set_password($user_data->{password});

	$user->check;
	$self->repo->save($user);

	# TODO: send an email

	return $user;
}

sub find_user_by_email ($self, $email)
{
	return $self->repo->load(User => {email => lc $email});
}

