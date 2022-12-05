package Service::Faker;

use My::Moose;
use all 'Model';
use Game::Helpers;
use Game::Config;
use Faker;
use Data::ULID::XS qw(ulid);

use header;

has injected 'character_service';

has field 'faker' => (
	constructed => ['Faker', 'en-us'],
);

sub fake_user ($self, $plaintext_password = $self->faker->user_password)
{
	return Model::User->new(
		email => $self->faker->internet_email_address,
		plaintext_password => $plaintext_password,
	);
}

sub fake_player ($self, $user_id = ulid)
{
	return Model::Player->new(
		user_id => $user_id,
	);
}

sub fake_character ($self, $player_id = ulid)
{
	return Model::Character->new(
		player_id => $player_id,
		class_id => lore_class('Assassin')->id,
		name => $self->faker->person_first_name,
	);
}

sub fake_npc ($self, $npc_id = ulid)
{
	...;
}

sub fake_variables ($self, $character_id = ulid)
{
	return Model::CharacterVariables->new(
		id => $character_id,
		experience => int(rand(10000)),
		location_id => Game::Config->config->{starting_location}->id,
		pos_x => rand(10),
		pos_y => rand(10),
		health => rand(200),
		energy => rand(200),
	);
}

