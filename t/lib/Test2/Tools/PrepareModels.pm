package Test2::Tools::PrepareModels;

use Exporter qw(import);
use Test2::API qw(context);

use header;

our @EXPORT = qw(
	prepare_model
);

sub prepare_model ($model)
{
	foreach my $attr ($model->meta->boolean_attrs) {
		my $name = $attr->name;
		$model->{$name} = $model->{$name} ? 1 : 0;
	}

	return $model;
}

