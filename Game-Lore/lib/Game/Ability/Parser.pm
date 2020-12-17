package Game::Ability::Parser;

use header;
use Game::Common;
use Game::Common::Container;
use Game::Ability::Compiled;
use Game::Ability::Compiled::Effect;

sub find_effect_type ($type)
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Ability::EffectType', 'EffectType/*.pm')};

	return $list->{$type};
}

sub find_attribute ($type)
{
	state $list =
		{map { $_->lore_id => $_->new } Game::Common->load_classes('Game::Ability::Attribute', 'Attribute/*.pm')};

	return $list->{$type};
}

sub parse ($class)
{
	my $data = resolve('game_data_repo')->load_ability_data;
	my %loaded;

	for my $row ($data->each) {
		if (!exists $loaded{$row->{id}}) {
			$loaded{$row->{id}} = Game::Ability::Compiled->new(
				attribute => find_attribute($row->{attribute}),
				$row->%{
					qw(
						id
						passive cost
						cooldown range
						target_self
						target_ally
						target_foe
						target_ground
						)
				}
			);
		}

		$loaded{$row->{id}}->group($row->{effect_group})->add_effect(
			Game::Ability::Compiled::Effect->new(
				effect_type => find_effect_type($row->{effect_type}),
				attribute => find_attribute($row->{effect_attribute}),
				$row->%{
					qw(
						value deviation
						)
				}
			)
		);
	}

	return \%loaded;
}

1;
