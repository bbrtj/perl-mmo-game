package Game::Character::Class;

use header;
use Moo::Role;
use Game::Common::Container;
use Game::Character::Class::Compiled;
use Game::Ability;

no header;

with 'Game::LoreElement';

sub get ($self, $class = undef)
{
	state $classes = $self->_parse;

	return defined $class ? $classes->{$class} : $classes;
}

sub _parse ($self)
{
	my $data = resolve('repo')->class_data->load;
	my %loaded;

	for my $row ($data->@*) {
		if (!exists $loaded{$row->{id}}) {
			$loaded{$row->{id}} = Game::Character::Class::Compiled->new(
				$row->%{
					qw(
						id playable
						base_health health_per_level
						base_health_regen health_regen_per_level
						base_focus focus_per_level
						base_focus_regen focus_regen_per_level
						base_stats stats_per_level
						)
				}
			);
		}

		$loaded{$row->{id}}->add_ability(
			Game::Ability->get($row->{ability})
		);
	}

	return \%loaded;
}

1;

