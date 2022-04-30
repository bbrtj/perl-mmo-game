package Web::HTMLFormPlugin;

use header;

use parent 'Form::Tiny::Plugin::Diva';

sub plugin ($self, $caller, $context)
{
	my $ftpd_config = $self->SUPER::plugin($caller, $context);
	push $ftpd_config->{meta_roles}->@*, __PACKAGE__;

	return $ftpd_config;
}

use Moo::Role;
BEGIN { header::no_experimental_warnings }

after setup => sub ($self) {
	$self->add_diva_config(label_class => 'col-sm-12 form-label');
	$self->add_hook(
		after_error => sub ($self, $error) {
			if (!($error->error isa 'i18n')) {
				$error->set_error(_t $error->error);
			}
		}
	);
};

