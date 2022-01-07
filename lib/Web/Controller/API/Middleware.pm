package Web::Controller::API::Middleware;

use My::Moose -constr;
use DI;

use header;

extends 'Web::Controller::Middleware';

# TODO: respond method
sub unauthorized ($self)
{
	$self->respond(0, 'login required');
	$self->rendered(401);
	return undef;
}

sub bad_request ($self)
{
	$self->respond(0, 'invalid request');
	$self->rendered(400);
	return undef;
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

