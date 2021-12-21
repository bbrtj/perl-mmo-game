package Model;

use Moo::Role;
use Model::Role::Dummy;
use Carp;
use Scalar::Util qw(blessed);

use header;

my %orm_mapping;
my %orm_mapping_reverse;

sub _tweak_model ($class)
{
	my @attributes = $class->_get_attributes;
	foreach my $attribute (@attributes) {
		my $name = $attribute->name;
		next if $name eq 'id';

		# set up the writer:
		# - if the writer exists, load its name
		# - if the writer doesn't exist:
		#   - set up _set_$name if the model is stored
		#   - set up set_$name otherwise
		my $writer_attr = $attribute->meta->find_attribute_by_name("writer");
		my $writer_name = "set_$name";
		if (my $writer_value = $writer_attr->get_value($attribute)) {
			$writer_name = $writer_value;
		}
		else {
			$writer_name = "_$writer_name"
				if $class->DOES('Model::Role::Stored');

			$writer_attr->set_value($attribute, $writer_name);
		}

		# set up hooks for dirty tracking:
		# - if the method exists for set_$name, add after modifier to it
		# - if the method doesn't exist, create a method that later calls the writer
		if ($class->DOES('Model::Role::Stored')) {
			my $facade_name = "set_$name";
			my $method = $class->meta->find_method_by_name($facade_name);

			if (defined $method) {
				$class->meta->add_after_method_modifier($facade_name, sub ($self, @args) {
					$self->_dirty->{$name} = 1;
				});
			}
			else {
				$class->meta->add_method($facade_name, sub ($self, @args) {
					$self->_dirty->{$name} = 1;
					return $self->$writer_name(@args);
				});
			}
		}
	}

	return;
}

sub _register ($class)
{
	if ($class =~ /Model::(.+)/) {
		my $resultset = "Schema::Result::$1";
		$orm_mapping{$class} = $resultset;
		$orm_mapping_reverse{$resultset} = $class;

		$class->_tweak_model;
		return $class->meta->make_immutable;
	}

	croak "cannot register $class";
}

sub _get_attributes ($self)
{
	return grep { $_->name !~ /^_/ } $self->meta->get_all_attributes;
}

sub serialize ($self)
{
	return {
		map {
			$_->name => $_->get_value($self)
		} grep {
			 $_->has_value($self)
		} $self->_get_attributes
	};
}

sub from_result ($class, $row)
{
	my $resultset = blessed $row;
	croak "invalid argument to from_result"
		unless defined $resultset;

	my $real_class = $orm_mapping_reverse{$resultset};
	return $real_class->new(
		map {
			my $name = $_->name;
			my $sub = $row->can($name);
			croak "cannot fetch $name from $resultset result set"
				unless $sub;

			$name => $sub->($row);
		} $real_class->_get_attributes
	);
}

sub get_result_class ($self)
{
	my $class = blessed $self // $self;

	croak 'invalid argument for get_result_class'
		unless exists $orm_mapping{$class};

	return $orm_mapping{$class};
}

sub bootstrap ($class)
{
	Game::Common->load_classes('Model', 'Model/*.pm');
	return;
}

sub dummy ($class)
{
	croak "dummy only works on class context" if ref $class;
	return Model::Role::Dummy->_make_dummy($class);
}

