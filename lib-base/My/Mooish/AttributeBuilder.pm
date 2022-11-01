package My::Mooish::AttributeBuilder;

use v5.36;

use parent 'Mooish::AttributeBuilder';

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

Mooish::AttributeBuilder::add_shortcut(sub ($name, %args) {
	if ($args{_type} eq 'injected') {
		require DI;

		my $as = (delete $args{as}) // $name;
		%args = (
			%args,
			DI->injected($as)
		);
	}

	return %args;
});

Mooish::AttributeBuilder::add_shortcut(sub ($name, %args) {
	state $map = {
		'[]' => 'Array',
		'{}' => 'Hash',
		'()' => 'Code',
		'""' => 'String',
		'..' => 'Counter',
		'!!' => 'Bool',
		'++' => 'Number',
	};

	for my $suffix (keys $map->%*) {
		next unless exists $args{'handles' . $suffix};
		my $handles = delete $args{'handles' . $suffix};
		my $type = $map->{$suffix};

		push $args{handles_via}->@*, $type;
		$args{handles} = {
			%{$args{handles} // {}},
			map {
				$_ => ($handles->{$_} =~ s/(\w)/${type}->$1/r)
			} keys $handles->%*
		};
	}

	return %args;
});

1;

