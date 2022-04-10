package Service::Faker;

use My::Moose;
use Model;
use Game::Helpers;

use header;

has 'repo' => (
	is => 'ro',
);

sub create_player ($self, $user)
{
	my $player = Model::Player->new(user_id => $user->id);
	$self->repo->save($player);

	my $character = Model::Character->new(
		player_id => $player->id,
		class_id => lore_class('Assassin')->id,
		name => $user->email =~ s/@.*$//r,
		stats => '',
	);
	$self->repo->save($character);

	return;
}

