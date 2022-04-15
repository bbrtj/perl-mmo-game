package Service::Faker;

use My::Moose;
use Model;
use Game::Helpers;

use header;

has 'character_service' => (
	is => 'ro',
);

sub fake_player ($self, $user)
{
	$self->character_service->create_player($user, {
		class => lore_class('Assassin'),
		name => $user->email =~ s/@.*$//r,
	});

	return;
}

