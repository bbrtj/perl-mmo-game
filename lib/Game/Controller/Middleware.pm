package Game::Controller::Middleware;

use Mojo::Base 'Mojolicious::Controller';
use DI;

use header;

sub unauthorized ($c)
{
	$c->render(json => {error => 'login required'}, status => 401);
	return undef;
}

sub bad_request ($c)
{
	$c->render(json => {error => 'invalid request'}, status => 400);
	return undef;
}

sub is_user ($c)
{
	my $user_id = $c->session('user');

	return unauthorized($c)
		unless $user_id;

	my $user = DI->get('schema_repo')->load(User => $user_id);

	return bad_request($c)
		unless $user;

	$c->stash(user => $user);
	return 1;
}

sub is_player ($c)
{
	my $player_id = $c->session('player');

	return unauthorized($c)
		unless $player_id;

	my $player = DI->get('schema_repo')->load(Player => $player_id);
	my $user = $c->stash('user');

	return bad_request($c)
		unless $player || $player->user_id ne $user->id;

	$c->stash(player => $player);
	return 1;
}
