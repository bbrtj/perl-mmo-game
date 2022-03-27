package Service::User;

use My::Moose;
use Model::User;
use Model::Player;
use Model::Character;
use Game::Helpers;

use header;

has 'repo' => (
	is => 'ro',
);

sub register_user ($self, $user_data)
{
	my $user = Model::User->new($user_data);
	$user->set_password($user_data->{password});

	$user->promote;
	$self->repo->save($user);

	# TODO: send an email

	# TODO: remove this after getting proper character creation

	my $player = Model::Player->new(user_id => $user->id);
	$self->repo->save($player);

	my $character = Model::Character->new(
		player_id => $player->id,
		class_id => lore_class('Assassin')->id,
		name => $user->email =~ s/@.*$//r,
		stats => '',
	);
	$self->repo->save($character);

	return $user;
}

sub find_user_by_email ($self, $email)
{
	return $self->repo->load(User => {email => lc $email});
}

