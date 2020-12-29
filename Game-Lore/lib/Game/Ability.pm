package Game::Ability;

use header;
use Moo::Role;
use Game::Common::Container;
use Game::Ability::Compiled;
use Game::Ability::Compiled::Effect;
use Game::Ability::EffectType;
use Game::Ability::Attribute;

no header;

with 'Game::LoreElement';

sub get ($self, $ability = undef)
{
	state $abilities = $self->_parse;

	return defined $ability ? $abilities->{$ability} : $abilities;
}

sub _parse ($self)
{
	my $data = resolve('repo')->ability_data->load;
	my %loaded;

	for my $row ($data->@*) {
		if (!exists $loaded{$row->{id}}) {
			$loaded{$row->{id}} = Game::Ability::Compiled->new(
				attribute => Game::Ability::Attribute->get($row->{attribute}),
				$row->%{
					qw(
						id cost
						instant
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
				effect_type => Game::Ability::EffectType->get($row->{effect_type}),
				attribute => Game::Ability::Attribute->get($row->{effect_attribute}),
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
