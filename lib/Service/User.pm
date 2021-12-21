package Service::User;

use Moo;
use Model::User;

use header;

has 'repo' => (
	is => 'ro',
);

sub register_user ($self, $user_data)
{
	my $user = Model::User->dummy->new($user_data);
	$user->set_password($user_data->{password});
	$user->promote;

	$self->repo->save($user);

	# send an email
	return $user;
}

sub find_user_by_email ($self, $email)
{
	return $self->repo->load(User => {email => lc $email});
}
