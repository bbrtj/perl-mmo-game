package Web::Controller::Middleware;

use My::Moose -constr;
use DI;

use header;

extends 'Mojolicious::Controller';

sub unauthorized ($self)
{
	$self->redirect_to('/user/login');
	return undef;
}

sub bad_request ($self)
{
	$self->render(text => 'Something went wrong, try again later');
	$self->rendered(400);
	return undef;
}

sub prepare_request ($self)
{
	my $user_id = $self->session->{user};
	my $user;

	$user = DI->get('schema_repo')->load(User => $user_id)
		if $user_id;
	$self->stash(user => $user);

	return 1;
}

sub is_user ($self)
{
	my $user_id = $self->session->{user};
	my $user = $self->stash('user');

	return $self->unauthorized
		unless $user_id;

	return $self->bad_request
		unless $user && $user->id eq $user_id;

	return 1;
}
