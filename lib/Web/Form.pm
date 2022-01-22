package Web::Form;

use Form::Tiny plugins => [qw(Diva)];

use header;

diva_config label_class => 'col-sm-12 form-label';

form_hook after_error => sub ($self, $error) {
	if (!($error->error isa 'i18n')) {
		$error->set_error(_t $error->error);
	}
};
