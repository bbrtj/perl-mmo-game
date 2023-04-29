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

has cached '_unique_seen' => (
	default => sub { {} },
);

sub _fake_unique ($self, @whats)
{
	my $what = join '-', @whats;
	$self->_unique_seen->{$what} //= {};
	my $category = $self->_unique_seen->{$what};

	my $faked;
	while (-not_unique) {
		$faked = '';
		foreach my $meth (@whats) {
			$faked .= $self->faker->$meth;
		}

		last unless $category->{$faked};
	}

	$category->{$faked} = 1;
	return $faked;
}

sub fake_user ($self, %params)
{
	return Model::User->new(
		email => $self->_fake_unique('internet_email_address'),
		plaintext_password => $self->faker->user_password,
		%params
	);
}

sub fake_player ($self, %params)
{
	return Model::Player->new(
		user_id => Types::ULID::ulid,
		%params
	);
}

sub fake_character ($self, %params)
{
	return Model::Character->new(
		player_id => Types::ULID::ulid,
		class_id => lore_class('Assassin')->id,
		name => $self->_fake_unique('person_first_name', 'person_last_name'),
		%params
	);
}

sub fake_npc ($self, %params)
{
	...;
}

sub fake_variables ($self, %params)
{
	return Model::CharacterVariables->new(
		id => Types::ULID::ulid,
		experience => int(rand(10000)),
		location_id => Game::Config->config->{starting_location}->id,
		pos_x => rand(Game::Config->config->{discover_radius} / sqrt(2)),
		pos_y => rand(Game::Config->config->{discover_radius} / sqrt(2)),
		health => rand(200),
		energy => rand(200),
		%params
	);
}

