package Game::Form::Role::HTML;

use Moo::Role;
use Form::Diva;

use header;

sub _diva_config ($self)
{
	if ($self->can('diva_config')) {
		return $self->diva_config;
	}

	return (
		label_class => 'col-sm-12 form-label',
		input_class => 'form-control',
	);
}

sub _tweak_diva ($self, $generated)
{
	my $errors = $self->errors_hash;
	for my $field ($generated->@*) {
		my $extra_data = $field->{comment};

		my $errors_html = join '', map { "<p>$_</p>" } $errors->{$extra_data->{n}}->@*;
		$field->{errors} = $errors_html;

		$field->{label} = ''
			if $extra_data->{nl};
	}

	return $generated;
}

sub form_html ($self)
{
	my @fields;
	my @hidden;
	for my $field ($self->field_defs->@*) {
		my %field_data = ( n => $field->name );
		if (defined $field) {
			%field_data = (
				%field_data,
				$field->data->%*
			);

			push @fields, { %field_data, comment => \%field_data };
		}
		else {
			push @hidden, { %field_data, comment => \%field_data };
		}
	}

	my $diva = Form::Diva->new(
		$self->_diva_config,
		form => \@fields,
		hidden => \@hidden,
	);

	my $generated = $diva->generate($self->input);

	return $self->_tweak_diva($generated);
}

