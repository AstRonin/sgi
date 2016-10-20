import pprint
import json
from cgi import parse_qs, escape


html = """<html>
        <head>
            <meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
            <link href='/static/synrc.css' type='text/css' rel='stylesheet'>
            <link href='http://fonts.googleapis.com/css?family=Open+Sans' rel='stylesheet' type='text/css'>
            <title>Chat</title>
        </head>
        <body>
            <h2  align='center'>Test page, 2 column, left with WebSocket instead of Ajax and right with just big text</h2>
            <table cellpadding='20' align='center'>
                <tr>
                    <td width='400' valign='top' height='550' >
                        <div id='new-form'>Wait 1 second, please...</div>
                    </td>
                    <td width='600' valign='top'>
                        <p>This some text:</p>
                        <p>Lorem ipsum dolor sit amet, dolor denique ponderum vis ei, mea eros minimum gloriatur eu, nec ei sanctus laboramus complectitur. Ubique abhorreant sed te, graeci blandit conceptam mel eu. Mea cetero nostrud nusquam ne, ex causae aliquando vix, aperiam civibus ne nam. Ornatus commune et sit, ad eum tale corpora definitiones, ea qui prima mundi mnesarchum. Ei mea esse eligendi omittantur, duo possit conceptam appellantur ne. Numquam maiorum per at, deleniti adipisci sea et.</p>
                        <p>An tation homero definitionem nec, id audiam alienum prodesset his. Cu mel tale posse. Ius magna pertinacia no, id persecuti disputationi nec. Tollit soluta everti mei ut.</p>
                        <p>His et hinc quaestio. Dictas facilisis ut mea, lucilius efficiendi cum te. Partiendo erroribus dignissim his id, quo aperiam appareat repudiare ei, vim no dico hinc debitis. Vide nihil salutatus cum cu, ne mandamus liberavisse pro. Sed at modo urbanitas, ius zril deserunt quaerendum ne. Eu quas dolorum ius, nec dolorum debitis invenire ei.</p>
                        <p>Melius inimicus has ad, augue melius sed no. Admodum salutatus sit ex. Nam ne erat phaedrum. Ne nec mediocrem laboramus, mei cu velit putant. Quis habeo graece in usu.</p>
                        <p>Singulis gloriatur mei ut, per ne atomorum vituperata delicatissimi. Cum iudico alterum accusata eu, qui rebum modus timeam ea, duo id clita expetenda. Vis persius noluisse facilisis ei, an per debitis nonumes qualisque. Magna invidunt ne vix. Et habemus nominati eloquentiam eum, duo no dicant audiam sapientem, no posse admodum vel.</p>
                        <p>Eu pri persius voluptatum definitionem, in equidem persequeris vim. Est te brute eripuit convenire, erant eirmod ea est. Vis et putant scaevola suscipiantur, vel ex agam complectitur. Tractatos similique ne sed.</p>
                        <p>Ut cum molestiae persecuti intellegam, no est autem tantas volutpat, soleat explicari ne sit. Duo ea debet splendide, sit cu dicunt possim, no per exerci philosophia. Summo repudiandae sea cu. Timeam diceret impedit est ne.</p>
                        <p>Et sea augue facilisis sadipscing. Usu scripta vituperata te, vide maluisset ne nam, mei eu choro utinam disputationi. Mel facer feugiat te, sumo repudiare eos ei. Elit inimicus consequuntur duo ut, at clita putant animal vix, qui cu nostro volutpat. Labitur discere postulant ne sed, affert honestatis neglegentur sea te. Veniam tibique consequuntur ad mei.</p>
                        <p>At has nominavi splendide expetendis, at vim vivendum insolens, te facete moderatius eum. An ius suavitate maluisset contentiones. Elitr verear perpetua sit at, in iudico dicunt pri, ridens suavitate ex his. Affert commodo neglegentur an eos, vim no facilisis torquatos. Eu eum alienum detracto.</p>
                        <p>Ne quot saepe nec, tempor laboramus ius ea, ex mei eripuit gubergren delicatissimi. Tota novum quando ut has, ei causae omnesque consequat vix, id ius oblique legimus. Duo ut dicant audire facilisis. Graeco voluptatum ei has, audiam veritus id nec.</p>
                        <p>Et per facer postea comprehensam, no albucius noluisse splendide mea. Agam tacimates inciderint vix cu, te ferri veritus mnesarchum qui. No duis fugit dissentiunt pri, minim pericula eos id. Ad iusto vidisse omittantur mel, audire oblique id eos. Utinam dictas periculis et mea. At everti nostrum sententiae qui, id clita utamur voluptaria quo.</p>
                        <p>Euismod graecis no vix, at quo vide philosophia, ad utamur accumsan dissentiet nec. Ea falli solet explicari mel. Minimum dignissim est ad, sed cu illud possit. Per no primis deterruisset, quis gloriatur id his. In libris concludaturque nec, tempor labitur officiis no ius. Eos laboramus adipiscing repudiandae ei, mei id unum rebum legendos.</p>
                        <p>Ornatus reformidans pri et, nisl ludus eloquentiam ius ei. Ea fierent ponderum vel, sit ne sint falli graecis. Pri eu soluta signiferumque, mel cu partiendo conceptam. Pro ea dicant everti, at nibh honestatis omittantur vel.</p>
                        <p>Est solet legimus contentiones et. Mea amet errem postea no, illud probatus interesset in usu, viderer laboramus nec cu. Ei congue iudicabit vel, elit deleniti qui ex, ei eos amet discere quaerendum. Reque aeque id cum, summo neglegentur ullamcorper has ne. Enim audire nec ea, sed tation laoreet scripserit et.</p>
                        <p>Mea ex phaedrum tacimates adversarium. Enim assentior ad pri. Ex accusam gubergren est. Ea nec case conclusionemque, mea ut brute maiestatis. At probo electram pertinacia vim.</p>
                        <p>Eam errem perfecto id, pri no labitur eligendi gubergren, vis ea augue solet. Eos omnes fabellas iudicabit at, et doming aliquip verterem pro. Cu omnis oratio nam, nec dicam dolores ut. Paulo evertitur id nam.</p>
                        <p>Dicam option quaerendum sit eu. Eu diam natum intellegat vim. Ea vel audiam aperiri conceptam. Modus doctus lobortis ius cu, vis animal persius urbanitas te. Pertinax indoctum eum in, homero labore percipitur cu mei. Officiis indoctum ocurreret eu nec, id nec quaestio repudiare.</p>
                        <p>Ut per suscipiantur definitionem. Ad veri expetendis vim, te sed erant placerat expetendis. Commodo voluptaria ius te. Ei inermis adolescens vel, vis semper noluisse suscipiantur eu, eam autem singulis at. Mea tamquam virtute ei.</p>
                        <p>Et alii primis reprimique usu. Mutat tempor eligendi sit cu, eu clita mollis facilisis quo, et posse simul vix. Sit minim rationibus ea, duo ea quando dictas. Pro ex errem impetus argumentum. Nostrud delectus sed ei, sed quas omnes officiis in.</p>
                        <p>Natum torquatos deterruisset ea mel, meliore forensibus vituperata ea eam. Volutpat contentiones his ei, at facete impetus blandit quo, pri ei porro utamur nusquam. Id vocent docendi complectitur quo, pro cu meliore scribentur. Et vel definitiones interpretaris, mea diam cetero ea. Inermis molestie mea at.</p>
                        <p>Lorem ipsum dolor sit amet, dolor denique ponderum vis ei, mea eros minimum gloriatur eu, nec ei sanctus laboramus complectitur. Ubique abhorreant sed te, graeci blandit conceptam mel eu. Mea cetero nostrud nusquam ne, ex causae aliquando vix, aperiam civibus ne nam. Ornatus commune et sit, ad eum tale corpora definitiones, ea qui prima mundi mnesarchum. Ei mea esse eligendi omittantur, duo possit conceptam appellantur ne. Numquam maiorum per at, deleniti adipisci sea et.</p>
                        <p>An tation homero definitionem nec, id audiam alienum prodesset his. Cu mel tale posse. Ius magna pertinacia no, id persecuti disputationi nec. Tollit soluta everti mei ut.</p>
                        <p>His et hinc quaestio. Dictas facilisis ut mea, lucilius efficiendi cum te. Partiendo erroribus dignissim his id, quo aperiam appareat repudiare ei, vim no dico hinc debitis. Vide nihil salutatus cum cu, ne mandamus liberavisse pro. Sed at modo urbanitas, ius zril deserunt quaerendum ne. Eu quas dolorum ius, nec dolorum debitis invenire ei.</p>
                        <p>Melius inimicus has ad, augue melius sed no. Admodum salutatus sit ex. Nam ne erat phaedrum. Ne nec mediocrem laboramus, mei cu velit putant. Quis habeo graece in usu.</p>
                        <p>Singulis gloriatur mei ut, per ne atomorum vituperata delicatissimi. Cum iudico alterum accusata eu, qui rebum modus timeam ea, duo id clita expetenda. Vis persius noluisse facilisis ei, an per debitis nonumes qualisque. Magna invidunt ne vix. Et habemus nominati eloquentiam eum, duo no dicant audiam sapientem, no posse admodum vel.</p>
                        <p>Eu pri persius voluptatum definitionem, in equidem persequeris vim. Est te brute eripuit convenire, erant eirmod ea est. Vis et putant scaevola suscipiantur, vel ex agam complectitur. Tractatos similique ne sed.</p>
                        <p>Ut cum molestiae persecuti intellegam, no est autem tantas volutpat, soleat explicari ne sit. Duo ea debet splendide, sit cu dicunt possim, no per exerci philosophia. Summo repudiandae sea cu. Timeam diceret impedit est ne.</p>
                        <p>Et sea augue facilisis sadipscing. Usu scripta vituperata te, vide maluisset ne nam, mei eu choro utinam disputationi. Mel facer feugiat te, sumo repudiare eos ei. Elit inimicus consequuntur duo ut, at clita putant animal vix, qui cu nostro volutpat. Labitur discere postulant ne sed, affert honestatis neglegentur sea te. Veniam tibique consequuntur ad mei.</p>
                        <p>At has nominavi splendide expetendis, at vim vivendum insolens, te facete moderatius eum. An ius suavitate maluisset contentiones. Elitr verear perpetua sit at, in iudico dicunt pri, ridens suavitate ex his. Affert commodo neglegentur an eos, vim no facilisis torquatos. Eu eum alienum detracto.</p>
                        <p>Ne quot saepe nec, tempor laboramus ius ea, ex mei eripuit gubergren delicatissimi. Tota novum quando ut has, ei causae omnesque consequat vix, id ius oblique legimus. Duo ut dicant audire facilisis. Graeco voluptatum ei has, audiam veritus id nec.</p>
                        <p>Et per facer postea comprehensam, no albucius noluisse splendide mea. Agam tacimates inciderint vix cu, te ferri veritus mnesarchum qui. No duis fugit dissentiunt pri, minim pericula eos id. Ad iusto vidisse omittantur mel, audire oblique id eos. Utinam dictas periculis et mea. At everti nostrum sententiae qui, id clita utamur voluptaria quo.</p>
                        <p>Euismod graecis no vix, at quo vide philosophia, ad utamur accumsan dissentiet nec. Ea falli solet explicari mel. Minimum dignissim est ad, sed cu illud possit. Per no primis deterruisset, quis gloriatur id his. In libris concludaturque nec, tempor labitur officiis no ius. Eos laboramus adipiscing repudiandae ei, mei id unum rebum legendos.</p>
                        <p>Ornatus reformidans pri et, nisl ludus eloquentiam ius ei. Ea fierent ponderum vel, sit ne sint falli graecis. Pri eu soluta signiferumque, mel cu partiendo conceptam. Pro ea dicant everti, at nibh honestatis omittantur vel.</p>
                        <p>Est solet legimus contentiones et. Mea amet errem postea no, illud probatus interesset in usu, viderer laboramus nec cu. Ei congue iudicabit vel, elit deleniti qui ex, ei eos amet discere quaerendum. Reque aeque id cum, summo neglegentur ullamcorper has ne. Enim audire nec ea, sed tation laoreet scripserit et.</p>
                        <p>Mea ex phaedrum tacimates adversarium. Enim assentior ad pri. Ex accusam gubergren est. Ea nec case conclusionemque, mea ut brute maiestatis. At probo electram pertinacia vim.</p>
                        <p>Eam errem perfecto id, pri no labitur eligendi gubergren, vis ea augue solet. Eos omnes fabellas iudicabit at, et doming aliquip verterem pro. Cu omnis oratio nam, nec dicam dolores ut. Paulo evertitur id nam.</p>
                        <p>Dicam option quaerendum sit eu. Eu diam natum intellegat vim. Ea vel audiam aperiri conceptam. Modus doctus lobortis ius cu, vis animal persius urbanitas te. Pertinax indoctum eum in, homero labore percipitur cu mei. Officiis indoctum ocurreret eu nec, id nec quaestio repudiare.</p>
                        <p>Ut per suscipiantur definitionem. Ad veri expetendis vim, te sed erant placerat expetendis. Commodo voluptaria ius te. Ei inermis adolescens vel, vis semper noluisse suscipiantur eu, eam autem singulis at. Mea tamquam virtute ei.</p>
                        <p>Et alii primis reprimique usu. Mutat tempor eligendi sit cu, eu clita mollis facilisis quo, et posse simul vix. Sit minim rationibus ea, duo ea quando dictas. Pro ex errem impetus argumentum. Nostrud delectus sed ei, sed quas omnes officiis in.</p>
                        <p>Natum torquatos deterruisset ea mel, meliore forensibus vituperata ea eam. Volutpat contentiones his ei, at facete impetus blandit quo, pri ei porro utamur nusquam. Id vocent docendi complectitur quo, pro cu meliore scribentur. Et vel definitiones interpretaris, mea diam cetero ea. Inermis molestie mea at.</p>
                        <p>Lorem ipsum dolor sit amet, dolor denique ponderum vis ei, mea eros minimum gloriatur eu, nec ei sanctus laboramus complectitur. Ubique abhorreant sed te, graeci blandit conceptam mel eu. Mea cetero nostrud nusquam ne, ex causae aliquando vix, aperiam civibus ne nam. Ornatus commune et sit, ad eum tale corpora definitiones, ea qui prima mundi mnesarchum. Ei mea esse eligendi omittantur, duo possit conceptam appellantur ne. Numquam maiorum per at, deleniti adipisci sea et.</p>
                        <p>An tation homero definitionem nec, id audiam alienum prodesset his. Cu mel tale posse. Ius magna pertinacia no, id persecuti disputationi nec. Tollit soluta everti mei ut.</p>
                        <p>His et hinc quaestio. Dictas facilisis ut mea, lucilius efficiendi cum te. Partiendo erroribus dignissim his id, quo aperiam appareat repudiare ei, vim no dico hinc debitis. Vide nihil salutatus cum cu, ne mandamus liberavisse pro. Sed at modo urbanitas, ius zril deserunt quaerendum ne. Eu quas dolorum ius, nec dolorum debitis invenire ei.</p>
                        <p>Melius inimicus has ad, augue melius sed no. Admodum salutatus sit ex. Nam ne erat phaedrum. Ne nec mediocrem laboramus, mei cu velit putant. Quis habeo graece in usu.</p>
                        <p>Singulis gloriatur mei ut, per ne atomorum vituperata delicatissimi. Cum iudico alterum accusata eu, qui rebum modus timeam ea, duo id clita expetenda. Vis persius noluisse facilisis ei, an per debitis nonumes qualisque. Magna invidunt ne vix. Et habemus nominati eloquentiam eum, duo no dicant audiam sapientem, no posse admodum vel.</p>
                        <p>Eu pri persius voluptatum definitionem, in equidem persequeris vim. Est te brute eripuit convenire, erant eirmod ea est. Vis et putant scaevola suscipiantur, vel ex agam complectitur. Tractatos similique ne sed.</p>
                        <p>Ut cum molestiae persecuti intellegam, no est autem tantas volutpat, soleat explicari ne sit. Duo ea debet splendide, sit cu dicunt possim, no per exerci philosophia. Summo repudiandae sea cu. Timeam diceret impedit est ne.</p>
                        <p>Et sea augue facilisis sadipscing. Usu scripta vituperata te, vide maluisset ne nam, mei eu choro utinam disputationi. Mel facer feugiat te, sumo repudiare eos ei. Elit inimicus consequuntur duo ut, at clita putant animal vix, qui cu nostro volutpat. Labitur discere postulant ne sed, affert honestatis neglegentur sea te. Veniam tibique consequuntur ad mei.</p>
                        <p>At has nominavi splendide expetendis, at vim vivendum insolens, te facete moderatius eum. An ius suavitate maluisset contentiones. Elitr verear perpetua sit at, in iudico dicunt pri, ridens suavitate ex his. Affert commodo neglegentur an eos, vim no facilisis torquatos. Eu eum alienum detracto.</p>
                        <p>Ne quot saepe nec, tempor laboramus ius ea, ex mei eripuit gubergren delicatissimi. Tota novum quando ut has, ei causae omnesque consequat vix, id ius oblique legimus. Duo ut dicant audire facilisis. Graeco voluptatum ei has, audiam veritus id nec.</p>
                        <p>Et per facer postea comprehensam, no albucius noluisse splendide mea. Agam tacimates inciderint vix cu, te ferri veritus mnesarchum qui. No duis fugit dissentiunt pri, minim pericula eos id. Ad iusto vidisse omittantur mel, audire oblique id eos. Utinam dictas periculis et mea. At everti nostrum sententiae qui, id clita utamur voluptaria quo.</p>
                        <p>Euismod graecis no vix, at quo vide philosophia, ad utamur accumsan dissentiet nec. Ea falli solet explicari mel. Minimum dignissim est ad, sed cu illud possit. Per no primis deterruisset, quis gloriatur id his. In libris concludaturque nec, tempor labitur officiis no ius. Eos laboramus adipiscing repudiandae ei, mei id unum rebum legendos.</p>
                        <p>Ornatus reformidans pri et, nisl ludus eloquentiam ius ei. Ea fierent ponderum vel, sit ne sint falli graecis. Pri eu soluta signiferumque, mel cu partiendo conceptam. Pro ea dicant everti, at nibh honestatis omittantur vel.</p>
                        <p>Est solet legimus contentiones et. Mea amet errem postea no, illud probatus interesset in usu, viderer laboramus nec cu. Ei congue iudicabit vel, elit deleniti qui ex, ei eos amet discere quaerendum. Reque aeque id cum, summo neglegentur ullamcorper has ne. Enim audire nec ea, sed tation laoreet scripserit et.</p>
                        <p>Mea ex phaedrum tacimates adversarium. Enim assentior ad pri. Ex accusam gubergren est. Ea nec case conclusionemque, mea ut brute maiestatis. At probo electram pertinacia vim.</p>
                        <p>Eam errem perfecto id, pri no labitur eligendi gubergren, vis ea augue solet. Eos omnes fabellas iudicabit at, et doming aliquip verterem pro. Cu omnis oratio nam, nec dicam dolores ut. Paulo evertitur id nam.</p>
                        <p>Dicam option quaerendum sit eu. Eu diam natum intellegat vim. Ea vel audiam aperiri conceptam. Modus doctus lobortis ius cu, vis animal persius urbanitas te. Pertinax indoctum eum in, homero labore percipitur cu mei. Officiis indoctum ocurreret eu nec, id nec quaestio repudiare.</p>
                        <p>Ut per suscipiantur definitionem. Ad veri expetendis vim, te sed erant placerat expetendis. Commodo voluptaria ius te. Ei inermis adolescens vel, vis semper noluisse suscipiantur eu, eam autem singulis at. Mea tamquam virtute ei.</p>
                        <p>Et alii primis reprimique usu. Mutat tempor eligendi sit cu, eu clita mollis facilisis quo, et posse simul vix. Sit minim rationibus ea, duo ea quando dictas. Pro ex errem impetus argumentum. Nostrud delectus sed ei, sed quas omnes officiis in.</p>
                        <p>Natum torquatos deterruisset ea mel, meliore forensibus vituperata ea eam. Volutpat contentiones his ei, at facete impetus blandit quo, pri ei porro utamur nusquam. Id vocent docendi complectitur quo, pro cu meliore scribentur. Et vel definitiones interpretaris, mea diam cetero ea. Inermis molestie mea at.</p>
                        <p>Lorem ipsum dolor sit amet, dolor denique ponderum vis ei, mea eros minimum gloriatur eu, nec ei sanctus laboramus complectitur. Ubique abhorreant sed te, graeci blandit conceptam mel eu. Mea cetero nostrud nusquam ne, ex causae aliquando vix, aperiam civibus ne nam. Ornatus commune et sit, ad eum tale corpora definitiones, ea qui prima mundi mnesarchum. Ei mea esse eligendi omittantur, duo possit conceptam appellantur ne. Numquam maiorum per at, deleniti adipisci sea et.</p>
                        <p>An tation homero definitionem nec, id audiam alienum prodesset his. Cu mel tale posse. Ius magna pertinacia no, id persecuti disputationi nec. Tollit soluta everti mei ut.</p>
                        <p>His et hinc quaestio. Dictas facilisis ut mea, lucilius efficiendi cum te. Partiendo erroribus dignissim his id, quo aperiam appareat repudiare ei, vim no dico hinc debitis. Vide nihil salutatus cum cu, ne mandamus liberavisse pro. Sed at modo urbanitas, ius zril deserunt quaerendum ne. Eu quas dolorum ius, nec dolorum debitis invenire ei.</p>
                        <p>Melius inimicus has ad, augue melius sed no. Admodum salutatus sit ex. Nam ne erat phaedrum. Ne nec mediocrem laboramus, mei cu velit putant. Quis habeo graece in usu.</p>
                        <p>Singulis gloriatur mei ut, per ne atomorum vituperata delicatissimi. Cum iudico alterum accusata eu, qui rebum modus timeam ea, duo id clita expetenda. Vis persius noluisse facilisis ei, an per debitis nonumes qualisque. Magna invidunt ne vix. Et habemus nominati eloquentiam eum, duo no dicant audiam sapientem, no posse admodum vel.</p>
                        <p>Eu pri persius voluptatum definitionem, in equidem persequeris vim. Est te brute eripuit convenire, erant eirmod ea est. Vis et putant scaevola suscipiantur, vel ex agam complectitur. Tractatos similique ne sed.</p>
                        <p>Ut cum molestiae persecuti intellegam, no est autem tantas volutpat, soleat explicari ne sit. Duo ea debet splendide, sit cu dicunt possim, no per exerci philosophia. Summo repudiandae sea cu. Timeam diceret impedit est ne.</p>
                        <p>Et sea augue facilisis sadipscing. Usu scripta vituperata te, vide maluisset ne nam, mei eu choro utinam disputationi. Mel facer feugiat te, sumo repudiare eos ei. Elit inimicus consequuntur duo ut, at clita putant animal vix, qui cu nostro volutpat. Labitur discere postulant ne sed, affert honestatis neglegentur sea te. Veniam tibique consequuntur ad mei.</p>
                        <p>At has nominavi splendide expetendis, at vim vivendum insolens, te facete moderatius eum. An ius suavitate maluisset contentiones. Elitr verear perpetua sit at, in iudico dicunt pri, ridens suavitate ex his. Affert commodo neglegentur an eos, vim no facilisis torquatos. Eu eum alienum detracto.</p>
                        <p>Ne quot saepe nec, tempor laboramus ius ea, ex mei eripuit gubergren delicatissimi. Tota novum quando ut has, ei causae omnesque consequat vix, id ius oblique legimus. Duo ut dicant audire facilisis. Graeco voluptatum ei has, audiam veritus id nec.</p>
                        <p>Et per facer postea comprehensam, no albucius noluisse splendide mea. Agam tacimates inciderint vix cu, te ferri veritus mnesarchum qui. No duis fugit dissentiunt pri, minim pericula eos id. Ad iusto vidisse omittantur mel, audire oblique id eos. Utinam dictas periculis et mea. At everti nostrum sententiae qui, id clita utamur voluptaria quo.</p>
                        <p>Euismod graecis no vix, at quo vide philosophia, ad utamur accumsan dissentiet nec. Ea falli solet explicari mel. Minimum dignissim est ad, sed cu illud possit. Per no primis deterruisset, quis gloriatur id his. In libris concludaturque nec, tempor labitur officiis no ius. Eos laboramus adipiscing repudiandae ei, mei id unum rebum legendos.</p>
                        <p>Ornatus reformidans pri et, nisl ludus eloquentiam ius ei. Ea fierent ponderum vel, sit ne sint falli graecis. Pri eu soluta signiferumque, mel cu partiendo conceptam. Pro ea dicant everti, at nibh honestatis omittantur vel.</p>
                        <p>Est solet legimus contentiones et. Mea amet errem postea no, illud probatus interesset in usu, viderer laboramus nec cu. Ei congue iudicabit vel, elit deleniti qui ex, ei eos amet discere quaerendum. Reque aeque id cum, summo neglegentur ullamcorper has ne. Enim audire nec ea, sed tation laoreet scripserit et.</p>
                        <p>Mea ex phaedrum tacimates adversarium. Enim assentior ad pri. Ex accusam gubergren est. Ea nec case conclusionemque, mea ut brute maiestatis. At probo electram pertinacia vim.</p>
                        <p>Eam errem perfecto id, pri no labitur eligendi gubergren, vis ea augue solet. Eos omnes fabellas iudicabit at, et doming aliquip verterem pro. Cu omnis oratio nam, nec dicam dolores ut. Paulo evertitur id nam.</p>
                        <p>Dicam option quaerendum sit eu. Eu diam natum intellegat vim. Ea vel audiam aperiri conceptam. Modus doctus lobortis ius cu, vis animal persius urbanitas te. Pertinax indoctum eum in, homero labore percipitur cu mei. Officiis indoctum ocurreret eu nec, id nec quaestio repudiare.</p>
                        <p>Ut per suscipiantur definitionem. Ad veri expetendis vim, te sed erant placerat expetendis. Commodo voluptaria ius te. Ei inermis adolescens vel, vis semper noluisse suscipiantur eu, eam autem singulis at. Mea tamquam virtute ei.</p>
                        <p>Et alii primis reprimique usu. Mutat tempor eligendi sit cu, eu clita mollis facilisis quo, et posse simul vix. Sit minim rationibus ea, duo ea quando dictas. Pro ex errem impetus argumentum. Nostrud delectus sed ei, sed quas omnes officiis in.</p>
                        <p>Natum torquatos deterruisset ea mel, meliore forensibus vituperata ea eam. Volutpat contentiones his ei, at facete impetus blandit quo, pri ei porro utamur nusquam. Id vocent docendi complectitur quo, pro cu meliore scribentur. Et vel definitiones interpretaris, mea diam cetero ea. Inermis molestie mea at.</p>
                        <p>Lorem ipsum dolor sit amet, dolor denique ponderum vis ei, mea eros minimum gloriatur eu, nec ei sanctus laboramus complectitur. Ubique abhorreant sed te, graeci blandit conceptam mel eu. Mea cetero nostrud nusquam ne, ex causae aliquando vix, aperiam civibus ne nam. Ornatus commune et sit, ad eum tale corpora definitiones, ea qui prima mundi mnesarchum. Ei mea esse eligendi omittantur, duo possit conceptam appellantur ne. Numquam maiorum per at, deleniti adipisci sea et.</p>
                        <p>An tation homero definitionem nec, id audiam alienum prodesset his. Cu mel tale posse. Ius magna pertinacia no, id persecuti disputationi nec. Tollit soluta everti mei ut.</p>
                        <p>His et hinc quaestio. Dictas facilisis ut mea, lucilius efficiendi cum te. Partiendo erroribus dignissim his id, quo aperiam appareat repudiare ei, vim no dico hinc debitis. Vide nihil salutatus cum cu, ne mandamus liberavisse pro. Sed at modo urbanitas, ius zril deserunt quaerendum ne. Eu quas dolorum ius, nec dolorum debitis invenire ei.</p>
                        <p>Melius inimicus has ad, augue melius sed no. Admodum salutatus sit ex. Nam ne erat phaedrum. Ne nec mediocrem laboramus, mei cu velit putant. Quis habeo graece in usu.</p>
                        <p>Singulis gloriatur mei ut, per ne atomorum vituperata delicatissimi. Cum iudico alterum accusata eu, qui rebum modus timeam ea, duo id clita expetenda. Vis persius noluisse facilisis ei, an per debitis nonumes qualisque. Magna invidunt ne vix. Et habemus nominati eloquentiam eum, duo no dicant audiam sapientem, no posse admodum vel.</p>
                        <p>Eu pri persius voluptatum definitionem, in equidem persequeris vim. Est te brute eripuit convenire, erant eirmod ea est. Vis et putant scaevola suscipiantur, vel ex agam complectitur. Tractatos similique ne sed.</p>
                        <p>Ut cum molestiae persecuti intellegam, no est autem tantas volutpat, soleat explicari ne sit. Duo ea debet splendide, sit cu dicunt possim, no per exerci philosophia. Summo repudiandae sea cu. Timeam diceret impedit est ne.</p>
                        <p>Et sea augue facilisis sadipscing. Usu scripta vituperata te, vide maluisset ne nam, mei eu choro utinam disputationi. Mel facer feugiat te, sumo repudiare eos ei. Elit inimicus consequuntur duo ut, at clita putant animal vix, qui cu nostro volutpat. Labitur discere postulant ne sed, affert honestatis neglegentur sea te. Veniam tibique consequuntur ad mei.</p>
                        <p>At has nominavi splendide expetendis, at vim vivendum insolens, te facete moderatius eum. An ius suavitate maluisset contentiones. Elitr verear perpetua sit at, in iudico dicunt pri, ridens suavitate ex his. Affert commodo neglegentur an eos, vim no facilisis torquatos. Eu eum alienum detracto.</p>
                        <p>Ne quot saepe nec, tempor laboramus ius ea, ex mei eripuit gubergren delicatissimi. Tota novum quando ut has, ei causae omnesque consequat vix, id ius oblique legimus. Duo ut dicant audire facilisis. Graeco voluptatum ei has, audiam veritus id nec.</p>
                        <p>Et per facer postea comprehensam, no albucius noluisse splendide mea. Agam tacimates inciderint vix cu, te ferri veritus mnesarchum qui. No duis fugit dissentiunt pri, minim pericula eos id. Ad iusto vidisse omittantur mel, audire oblique id eos. Utinam dictas periculis et mea. At everti nostrum sententiae qui, id clita utamur voluptaria quo.</p>
                        <p>Euismod graecis no vix, at quo vide philosophia, ad utamur accumsan dissentiet nec. Ea falli solet explicari mel. Minimum dignissim est ad, sed cu illud possit. Per no primis deterruisset, quis gloriatur id his. In libris concludaturque nec, tempor labitur officiis no ius. Eos laboramus adipiscing repudiandae ei, mei id unum rebum legendos.</p>
                        <p>Ornatus reformidans pri et, nisl ludus eloquentiam ius ei. Ea fierent ponderum vel, sit ne sint falli graecis. Pri eu soluta signiferumque, mel cu partiendo conceptam. Pro ea dicant everti, at nibh honestatis omittantur vel.</p>
                        <p>Est solet legimus contentiones et. Mea amet errem postea no, illud probatus interesset in usu, viderer laboramus nec cu. Ei congue iudicabit vel, elit deleniti qui ex, ei eos amet discere quaerendum. Reque aeque id cum, summo neglegentur ullamcorper has ne. Enim audire nec ea, sed tation laoreet scripserit et.</p>
                        <p>Mea ex phaedrum tacimates adversarium. Enim assentior ad pri. Ex accusam gubergren est. Ea nec case conclusionemque, mea ut brute maiestatis. At probo electram pertinacia vim.</p>
                        <p>Eam errem perfecto id, pri no labitur eligendi gubergren, vis ea augue solet. Eos omnes fabellas iudicabit at, et doming aliquip verterem pro. Cu omnis oratio nam, nec dicam dolores ut. Paulo evertitur id nam.</p>
                        <p>Dicam option quaerendum sit eu. Eu diam natum intellegat vim. Ea vel audiam aperiri conceptam. Modus doctus lobortis ius cu, vis animal persius urbanitas te. Pertinax indoctum eum in, homero labore percipitur cu mei. Officiis indoctum ocurreret eu nec, id nec quaestio repudiare.</p>
                        <p>Ut per suscipiantur definitionem. Ad veri expetendis vim, te sed erant placerat expetendis. Commodo voluptaria ius te. Ei inermis adolescens vel, vis semper noluisse suscipiantur eu, eam autem singulis at. Mea tamquam virtute ei.</p>
                        <p>Et alii primis reprimique usu. Mutat tempor eligendi sit cu, eu clita mollis facilisis quo, et posse simul vix. Sit minim rationibus ea, duo ea quando dictas. Pro ex errem impetus argumentum. Nostrud delectus sed ei, sed quas omnes officiis in.</p>
                        <p>Natum torquatos deterruisset ea mel, meliore forensibus vituperata ea eam. Volutpat contentiones his ei, at facete impetus blandit quo, pri ei porro utamur nusquam. Id vocent docendi complectitur quo, pro cu meliore scribentur. Et vel definitiones interpretaris, mea diam cetero ea. Inermis molestie mea at.</p>
                        <p>Lorem ipsum dolor sit amet, dolor denique ponderum vis ei, mea eros minimum gloriatur eu, nec ei sanctus laboramus complectitur. Ubique abhorreant sed te, graeci blandit conceptam mel eu. Mea cetero nostrud nusquam ne, ex causae aliquando vix, aperiam civibus ne nam. Ornatus commune et sit, ad eum tale corpora definitiones, ea qui prima mundi mnesarchum. Ei mea esse eligendi omittantur, duo possit conceptam appellantur ne. Numquam maiorum per at, deleniti adipisci sea et.</p>
                        <p>An tation homero definitionem nec, id audiam alienum prodesset his. Cu mel tale posse. Ius magna pertinacia no, id persecuti disputationi nec. Tollit soluta everti mei ut.</p>
                        <p>His et hinc quaestio. Dictas facilisis ut mea, lucilius efficiendi cum te. Partiendo erroribus dignissim his id, quo aperiam appareat repudiare ei, vim no dico hinc debitis. Vide nihil salutatus cum cu, ne mandamus liberavisse pro. Sed at modo urbanitas, ius zril deserunt quaerendum ne. Eu quas dolorum ius, nec dolorum debitis invenire ei.</p>
                        <p>Melius inimicus has ad, augue melius sed no. Admodum salutatus sit ex. Nam ne erat phaedrum. Ne nec mediocrem laboramus, mei cu velit putant. Quis habeo graece in usu.</p>
                        <p>Singulis gloriatur mei ut, per ne atomorum vituperata delicatissimi. Cum iudico alterum accusata eu, qui rebum modus timeam ea, duo id clita expetenda. Vis persius noluisse facilisis ei, an per debitis nonumes qualisque. Magna invidunt ne vix. Et habemus nominati eloquentiam eum, duo no dicant audiam sapientem, no posse admodum vel.</p>
                        <p>Eu pri persius voluptatum definitionem, in equidem persequeris vim. Est te brute eripuit convenire, erant eirmod ea est. Vis et putant scaevola suscipiantur, vel ex agam complectitur. Tractatos similique ne sed.</p>
                        <p>Ut cum molestiae persecuti intellegam, no est autem tantas volutpat, soleat explicari ne sit. Duo ea debet splendide, sit cu dicunt possim, no per exerci philosophia. Summo repudiandae sea cu. Timeam diceret impedit est ne.</p>
                        <p>Et sea augue facilisis sadipscing. Usu scripta vituperata te, vide maluisset ne nam, mei eu choro utinam disputationi. Mel facer feugiat te, sumo repudiare eos ei. Elit inimicus consequuntur duo ut, at clita putant animal vix, qui cu nostro volutpat. Labitur discere postulant ne sed, affert honestatis neglegentur sea te. Veniam tibique consequuntur ad mei.</p>
                        <p>At has nominavi splendide expetendis, at vim vivendum insolens, te facete moderatius eum. An ius suavitate maluisset contentiones. Elitr verear perpetua sit at, in iudico dicunt pri, ridens suavitate ex his. Affert commodo neglegentur an eos, vim no facilisis torquatos. Eu eum alienum detracto.</p>
                        <p>Ne quot saepe nec, tempor laboramus ius ea, ex mei eripuit gubergren delicatissimi. Tota novum quando ut has, ei causae omnesque consequat vix, id ius oblique legimus. Duo ut dicant audire facilisis. Graeco voluptatum ei has, audiam veritus id nec.</p>
                        <p>Et per facer postea comprehensam, no albucius noluisse splendide mea. Agam tacimates inciderint vix cu, te ferri veritus mnesarchum qui. No duis fugit dissentiunt pri, minim pericula eos id. Ad iusto vidisse omittantur mel, audire oblique id eos. Utinam dictas periculis et mea. At everti nostrum sententiae qui, id clita utamur voluptaria quo.</p>
                        <p>Euismod graecis no vix, at quo vide philosophia, ad utamur accumsan dissentiet nec. Ea falli solet explicari mel. Minimum dignissim est ad, sed cu illud possit. Per no primis deterruisset, quis gloriatur id his. In libris concludaturque nec, tempor labitur officiis no ius. Eos laboramus adipiscing repudiandae ei, mei id unum rebum legendos.</p>
                        <p>Ornatus reformidans pri et, nisl ludus eloquentiam ius ei. Ea fierent ponderum vel, sit ne sint falli graecis. Pri eu soluta signiferumque, mel cu partiendo conceptam. Pro ea dicant everti, at nibh honestatis omittantur vel.</p>
                        <p>Est solet legimus contentiones et. Mea amet errem postea no, illud probatus interesset in usu, viderer laboramus nec cu. Ei congue iudicabit vel, elit deleniti qui ex, ei eos amet discere quaerendum. Reque aeque id cum, summo neglegentur ullamcorper has ne. Enim audire nec ea, sed tation laoreet scripserit et.</p>
                        <p>Mea ex phaedrum tacimates adversarium. Enim assentior ad pri. Ex accusam gubergren est. Ea nec case conclusionemque, mea ut brute maiestatis. At probo electram pertinacia vim.</p>
                        <p>Eam errem perfecto id, pri no labitur eligendi gubergren, vis ea augue solet. Eos omnes fabellas iudicabit at, et doming aliquip verterem pro. Cu omnis oratio nam, nec dicam dolores ut. Paulo evertitur id nam.</p>
                        <p>Dicam option quaerendum sit eu. Eu diam natum intellegat vim. Ea vel audiam aperiri conceptam. Modus doctus lobortis ius cu, vis animal persius urbanitas te. Pertinax indoctum eum in, homero labore percipitur cu mei. Officiis indoctum ocurreret eu nec, id nec quaestio repudiare.</p>
                        <p>Ut per suscipiantur definitionem. Ad veri expetendis vim, te sed erant placerat expetendis. Commodo voluptaria ius te. Ei inermis adolescens vel, vis semper noluisse suscipiantur eu, eam autem singulis at. Mea tamquam virtute ei.</p>
                        <p>Et alii primis reprimique usu. Mutat tempor eligendi sit cu, eu clita mollis facilisis quo, et posse simul vix. Sit minim rationibus ea, duo ea quando dictas. Pro ex errem impetus argumentum. Nostrud delectus sed ei, sed quas omnes officiis in.</p>
                        <p>Natum torquatos deterruisset ea mel, meliore forensibus vituperata ea eam. Volutpat contentiones his ei, at facete impetus blandit quo, pri ei porro utamur nusquam. Id vocent docendi complectitur quo, pro cu meliore scribentur. Et vel definitiones interpretaris, mea diam cetero ea. Inermis molestie mea at.</p>

                    </td>
                </tr>
            </table>
            <script>var transition = {pid: '', host: 'localhost', port:'8000'};</script>
            <script src='/n2o/protocols/bert.js'></script>
            <script src='/n2o/protocols/binary.js'></script>
            <script src='/n2o/protocols/client.js'></script>
            <script src='/n2o/protocols/nitrogen.js'></script>
            <script src='/n2o/bullet.js'></script>
            <script src='/n2o/xhr.js'></script>
            <script src='/n2o/utf8.js'></script>
            <script src='/n2o/template.js'></script>
            <script src='/n2o/n2o.js'></script>
            <script src='/n2o/validation.js'></script>
            <script src='/n2o/ftp.js'></script>
            <script src='/n2o/http.js'></script>
            <!--script src='https://synrc.com/hi.js'></script-->
            <script>
            querystring = '/index-ws';
            protos = [$client,$bert]; N2O_start();</script>
            <script>
                setTimeout(function(){ // because need time to start ws.
                    http.send('/?page=form').done(function(data) {
                        // much more better use some jQuery
                        var n = document.createElement("div"); n.innerHTML = data;
                        var arr = n.getElementsByTagName('script');
                        for (var i = 0; i < arr.length; i++) eval(arr[i].innerHTML);
                        document.getElementById('new-form').innerHTML = data;
                    });
                }, 2000);
            </script>
        </body>
    </html>
"""

html_form = """<script>
	sendTestForm = function (obj) {
        var settings = {
            url: '/?page=json',
            method: 'POST',
            body: 'username='+obj.elements['test-username'].value+'&password='+obj.elements['test-password'].value,
            returnType: 'json'
        };
        http.send(settings).done(function (data, status, headers) {
            if (data) {
                document.getElementById('test-username').value = data.username;
                document.getElementById('test-password').value = data.password;
            }
        })
        .fail(function (data, status, headers) {
            console.error(data);
        });
    };
</script>

<h2>Test form send data to server by uwsgi protocol (to Python)</h2>
<div style="margin: 10px;">
    <form action="" method="post" onsubmit="sendTestForm(this);return false;">
        <div>
            <label for="username">Username</label>
            <input type="text" id="username" class="" name="test-username">
        </div>
        <div>
            <label for="password">Password</label>
            <input type="password" id="password" class="" name="test-password">
        </div>

        <div>
            <button type="submit" class="" name="login-button">Send Data to WS</button>
        </div>
    </form>
</div>
<div style="margin: 10px;">
    <h4>Your inputted data</h4>
    <div>
        <label>Username</label>
        <input type="text" id="test-username">
    </div>
    <div>
        <label>Password</label>
        <input type="text" id="test-password">
    </div>
</div>
"""

def application(env, start_response):
    # start_response('200 OK', [('Content-Type','text/html')])
    pprint.pprint(env['QUERY_STRING'])

    d = parse_qs(env['QUERY_STRING'])
    page = d.get('page', [''])[0]
    page = escape(page)

    return_data = html

    if page == 'form':
        return_data = form(env)
    if page == 'json':
        return_data = get_post(env)

    status = '200 OK'

    response_headers = [
        ('Content-Type', 'text/html'),
        ('Content-Length', str(len(return_data)))
    ]
    start_response(status, response_headers)

    return [return_data]


def form(env):
    return html_form

def get_post(env):

    try:
        request_body_size = int(env.get('CONTENT_LENGTH', 0))
    except (ValueError):
        request_body_size = 0

    request_body = env['wsgi.input'].read(request_body_size)

    d = parse_qs(request_body)

    us = 'username'
    ps = 'password'

    u = d.get(us, [''])[0]
    p = d.get(ps, [''])[0]

    return json.dumps({us: u, ps: p})

