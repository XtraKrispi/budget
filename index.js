import { Elm } from "./src/Main.elm";
import { createClient } from '@supabase/supabase-js';

const app = Elm.Main.init();

const supabase = createClient('https://cpmycpfuezkmecrryepz.supabase.co', 'sb_publishable_-lju4m81D5thK2S-E7ITDQ_3lNJXLgX')

app.ports.openDialog.subscribe((id) => {
    document.getElementById(id).showModal();
});

app.ports.signUp.subscribe(async ({ email, password }) => {
    const results = await supabase.auth.signUp({ email: email, password: password });
    if (results.error) {
        app.ports.onSignUpFailure.send(results.error.message);
    } else
        app.ports.onSignUpSuccess.send({ email: email, userId: results.id, confirmed: false });
});

app.ports.signIn.subscribe(async ({ email, password }) => {
    const results = await supabase.auth.signInWithPassword({ email: email, password: password });
    if (results.error) {
        app.ports.onLoginFailure.send(results.error.message);
    } else {
        app.ports.onLoginSuccess.send({ email: email, userId: results.data.user.id, confirmed: !!results.data.user.email_confirmed_at });
    }
});

app.ports.initiateGetUser.subscribe(async () => {
    const results = await supabase.auth.getUser();
    if (results.error) {
        app.ports.gotUser.send(null);
    } else {
        app.ports.gotUser.send({ email: results.data.user.email, userId: results.data.user.id, confirmed: !!results.data.user.email_confirmed_at });
    }

});

app.ports.copyToClipboard.subscribe((text) => {
    if ('clipboard' in window.navigator) {
        navigator.clipboard.writeText(text);
    }
});

app.ports.fetchDefinitions.subscribe(async () => {
    const { data, error } = await supabase.from("definitions").select();
    if (error) {
        app.ports.fetchDefinitionsFailure.send(error.message);
    } else {
        app.ports.fetchDefinitionsSuccess.send(data.map((def) => ({
            startDate: def.start_date
            , endDate: def.end_date
            , description: def.description
            , amount: def.amount
            , frequency: def.frequency
            , isAutomatic: def.is_automatic_withdrawal
        })));
    }
});

app.ports.fetchScratch.subscribe(async () => {
    const { data, error } = await supabase.from("scratch").select().limit(1).maybeSingle();
    if (error) {
        app.ports.fetchScratchFailure.send(error.message);
    } else {
        if (data) {
            app.ports.fetchScratchSuccess.send({
                endDate: data.end_date,
                amountInBank: data.amount_in_bank,
                amountLeftOver: data.amount_left_over
            });

        } else {
            app.ports.fetchScratchSuccess.send(null);
        }
    }
});