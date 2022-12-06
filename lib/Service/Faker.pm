package Service::Faker;

use My::Moose;
use all 'Model';
use Game::Helpers;
use Game::Config;
use Faker;

use header;

has injected 'character_service';

has field 'faker' => (
	constructed => ['Faker', 'en-us'],
);

sub fake_user ($self, $plaintext_password = $self->faker->user_password, %params)
{
	return Model::User->new(
		email => $self->faker->internet_email_address,
		plaintext_password => $plaintext_password,
		%params
	);
}

sub fake_player ($self, $user_id = Types::ULID::ulid, %params)
{
	return Model::Player->new(
		user_id => $user_id,
		%params
	);
}

sub fake_character ($self, $player_id = Types::ULID::ulid, %params)
{
	return Model::Character->new(
		player_id => $player_id,
		class_id => lore_class('Assassin')->id,
		name => $self->faker->person_first_name,
		%params
	);
}

sub fake_npc ($self, $npc_id = Types::ULID::ulid)
{
	...;
}

sub fake_variables ($self, $character_id = Types::ULID::ulid, %params)
{
	return Model::CharacterVariables->new(
		id => $character_id,
		experience => int(rand(10000)),
		location_id => Game::Config->config->{starting_location}->id,
		pos_x => rand(10),
		pos_y => rand(10),
		health => rand(200),
		energy => rand(200),
		%params
	);
}

