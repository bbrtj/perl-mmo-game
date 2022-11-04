package My::Mooish::AttributeBuilder;

use v5.36;

use parent 'Mooish::AttributeBuilder';
use Types::Standard qw(InstanceOf);
use Module::Load qw(load);

sub attribute_types ($self)
{
	my $std = $self->SUPER::attribute_types;
	return {
		%{$std},
		injected => {
			is => 'ro',
		}
	};
}

Mooish::AttributeBuilder::add_shortcut(
	sub ($name, %args) {
		if ($args{_type} eq 'injected') {
			require DI;

			my $aliasing = (delete $args{aliasing}) // $name;
			%args = (
				%args,
				DI->injected($aliasing)
			);
		}

		return %args;
	}
);

Mooish::AttributeBuilder::add_shortcut(
	sub ($name, %args) {
		if (my $constructed = delete $args{constructed}) {
			my ($class, @args) = $constructed->@*;

			load $class;

			Mooish::AttributeBuilder::check_and_set(
				\%args, $name,
				isa => InstanceOf [$class],
				default => sub { $class->new(@args) },
			);
		}

		return %args;
	}
);

Mooish::AttributeBuilder::add_shortcut(
	sub ($name, %args) {
		state $map = {
			'[]' => 'Array',
			'{}' => 'Hash',
			'()' => 'Code',
			'""' => 'String',
			'..' => 'Counter',
			'!!' => 'Bool',
			'++' => 'Number',
			'->' => 'Blessed',
		};

		for my $suffix (keys $map->%*) {
			next unless exists $args{'handles' . $suffix};
			my $handles = delete $args{'handles' . $suffix};
			my $type = $map->{$suffix};

			push $args{handles_via}->@*, $type;
			$args{handles} = {
				%{$args{handles} // {}},
				map {
					my $value = $handles->{$_};
					$_ => (ref $value ? $value : $value =~ s/(\w)/${type}->$1/r)
				} keys $handles->%*
			};
		}

		return %args;
	}
);

1;

