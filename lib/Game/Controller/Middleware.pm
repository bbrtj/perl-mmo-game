package Game::Controller::Middleware;

use Moo;
use DI;

use header;

extends 'Mojolicious::Controller';

sub unauthorized ($self)
{
	$self->render(json => {error => 'login required'}, status => 401);
	return undef;
}

sub bad_request ($self)
{
	$self->render(json => {error => 'invalid request'}, status => 400);
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

sub is_player ($self)
{
	my $player_id = $self->session->{player};

	return $self->unauthorized
		unless $player_id;

	my $player = DI->get('schema_repo')->load(Player => $player_id);
	my $user = $self->stash('user');

	return $self->bad_request
		unless $player || $player->user_id ne $user->id;

	$self->stash(player => $player);
	return 1;
}

