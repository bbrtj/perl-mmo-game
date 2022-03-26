package Game::LoreLoader::DSL;

use Game::LoreLoader;
use Game::LoreLoader::LoreDummy;
use Game::Config;

use Sub::Util qw(set_subname);

use header -noclean;

use constant TYPES => [qw(
	attribute
	race
	class
	item
	slot
	primary_stat
	secondary_stat
)];

use constant CONFIGS => [qw(
	translations
	define
	uses
	type
	subtype_of
)];

sub transform_name ($self, $name)
{
	$name = ucfirst $name;
	$name =~ s/_([a-z])/uc $1/eg;

	return $name;
}

sub get_dsl ($self)
{
	my @items;

	my sub reporter ($text) {
		my $context = $items[-1];
		$context = $context ? ref $context : '(no context)';
		die sprintf $text, $context;
	}

	my %dsl = (
		lore => sub ($id, $el) {
			my $class =  $el->class;
			eval "require $class";
			push @items, $class->new(
				id => $id,
				name => $el->name,
			);

			return;
		},
		requires => sub ($dependency) {
			Game::LoreLoader->load($dependency);
		},
	);

	for my $type (TYPES->@*) {
		my $class = 'Game::Lore::' . $self->transform_name($type);

		$dsl{$type} = sub :prototype($) ($name) {
			return Game::LoreLoader::LoreDummy->new(
				class => $class,
				name => $name,
			);
		};
	}

	for my $config (CONFIGS->@*) {
		$dsl{$config} = sub (@values) {
			my $context = $items[-1] // 'Game::Config';

			$context = $context->data
				unless $context->can($config);

			reporter "Context %s cannot utilize $config"
				unless $context->can($config);

			my $storage = $context->$config;

			if (is_hashref $storage) {
				if (@values == 1 && is_hashref $values[0]) {
					@values = $values[0]->%*;
				}

				my %kv_values = @values;
				for my $key (keys %kv_values) {
					reporter "replacing $key for $config in %s"
						if defined $storage->{$key};

					my $value = $kv_values{$key};
					$value = $value->create
						if $value->$_isa('Game::LoreLoader::LoreDummy');

					$storage->{$key} = $value;
				}
			}

			elsif (is_arrayref $storage) {
				for my $value (@values) {
					$value = $value->create
						if $value->$_isa('Game::LoreLoader::LoreDummy');

					push $storage->@*, $value;
				}
			}

			else {
				reporter "invalid data for plain $config for %s"
					unless @values == 1;

				my $value = $values[0];
				$value = $value->create
					if $value->$_isa('Game::LoreLoader::LoreDummy');

				my $setter = "set_$config";
				$context->$setter($value);
			}
		};
	}

	return %dsl;
}

sub import ($self, @args)
{
	my $package = caller;
	my %dsl = $self->get_dsl;

	for my $name (keys %dsl) {
		no strict 'refs';

		set_subname $name, $dsl{$name};
		*{"${package}::${name}"} = $dsl{$name};
	}

	return;
}

