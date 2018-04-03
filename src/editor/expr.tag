// Expr

import Mixin from './mixin.js'
import I from 'Block.Bridge'

// Bind ------------------------------------------------------------------------

<bind>
  <div>
    <bind-left data={data.value0} renew={renewL}/>
    <span class='token'>=</span>
  </div>
  <expr data={data.value1} outer={true}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.renewL  = d => this.renew(d && I.renewLeft(d)(this.data))
    this.onrenew = d => this.renew(I.renewRight(d)(this.data))
  </script>
</bind>

<bind-left class='var'>
  <!-- <func-sample data={var}/>
  <div class='term var {args.length == 0 ?"right":""}'>
    <span class='token'>{var.value0.value0}</span>
</div> -->
  <bind-var data={var} outer={args.length == 0}/>
  <pattern each={d, i in args} data={d} right={i == args.length - 1} renew={renewA(i)}/>
  <add-area renew={addArg} name='expr'/>

  <script>
    this.mixin(Mixin.Data)
    this.args = I.appToArray(this.opts.data)
    this.var  = this.args.shift()

    this.onrenew = d => this.renew(d && I.toApp(d)(this.args))
    this.renewA  = i => d => this.renew(d ? I.renewArgs(i)(d)(this.args)(this.var)
                                          : I.deleteArg(i)(this.args)(this.var) )
    this.addArg  = d => this.renew(I.toApp(this.var)(this.args.concat(d || I.pempty)))
  </script>
</bind-left>

<bind-var class={func: func, outer: opts.outer}>
  <div ref='slot' class='slot'>
    <div class='sample' if={cons == 'var'}>
      <div class='term var'>
        <handle remove={remove}/>
        <input-field data={expr.value0}/>
        <!--<span class='token'>{expr.value0}</span>-->
      </div>
      <hole each={t, i in holes} data={t} right={i == holes.length - 1} spine='var' conpact={false}/>
    </div>
    <div class='sample' if={cons == 'ope'}>
      <hole data={holes[0]} spine='opv' left={true} conpact={false}/>
      <div class='term opv'>
        <input-field data={expr.value0}/>
      </div>
      <hole data={holes[1]} spine='opv' right={true} conpact={false}/>
    </div>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Clonable)

    this.name   = 'expr'
    this.cons   = I.econs(this.data.value0)
    this.expr   = this.cons == 'ope' ? this.data.value0.value0.value0 : this.data.value0
    this.scheme = this.data.value1
    this.holes  = I.arrowToArray(this.scheme.value1)
    this.holes.pop()
    this.func   = this.holes.length > 0

    this.onrenew = s => this.renew(I.renewExpr(I.varC(s))(this.data))
    this.remove  = () => this.renew(null)
  </script>
</bind-var>

// Expr ------------------------------------------------------------------------

<expr class={left: opts.left, right: opts.right, outer: outer, func: func,
             bracket: bracket, factor: factor, hole: opts.hole}>
  <div ref='slot' class='slot draggable'>
    <expr-emp    if={cons == 'emp'} data={scheme.value1} delete={opts.delete}/>
    <var-expr    if={cons == 'var'} data={expr}/>
    <app-expr    if={cons == 'app'} data={expr} spine={spine} outer={outer && !func} renewe={renewE}/>
    <expr-lambda if={cons == 'lam'} data={expr} renew={renewE}/>
    <expr-oper   if={cons == 'ope'} data={expr}/>
    <num-expr    if={cons == 'num'} data={expr}/>
    <expr-if     if={cons == 'ift'} data={expr}/>
    <expr-case   if={cons == 'cas'} data={expr}/>
    <expr-let    if={cons == 'let'} data={expr}/>
  </div>
  <hole if={outer && func && !(factor && opts.left) && !empty} spine={spine} fill={parent.opts.fill}
        each={t, i in holes} data={t} right={i == holes.length - 1} renew={apply(i)}/>
  <add-area if={cons != 'app' && cons != 'emp' && lcons == 'var'} renew={addArg} name='expr'/>

  <infos show={hover} data={infos}/>
  <type-info show={hover} data={scheme}/>
  <highlight hover={hover} infos={infos}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Draggable)
    this.name = 'expr'

    this.expr    = this.data.value0
    this.cons    = I.econs(this.expr)
    this.scheme  = this.data.value1
    this.infos   = this.data.value2
    this.holes   = I.arrowToArray(this.scheme.value1);
    this.last    = this.holes.pop()
    this.lcons   = I.tcons(this.last.value0)
    this.oper    = this.cons == 'ope'
    this.empty   = this.cons == 'emp'
    this.func    = this.holes.length > 0 && !(this.oper && this.holes.length <= 2)
    this.app     = this.cons == 'app' && !opts.spine
    this.factor  = this.cons == 'lam' || this.cons == 'ift' || this.cons == 'cas' || this.cons == 'let'
    // this.oper    = this.cons == 'ope'
    this.bracket = (opts.bracket || opts.hole) && (this.func || this.app || this.oper) && !this.empty
                || this.factor && (opts.left || opts.bracket || opts.hole)
    this.outer   = opts.outer || this.bracket || this.app
    const spineC = I.econs(I.appToArray(this.data)[0].value0)
    this.spine   = opts.spine || (spineC == 'lam' || spineC == 'ift' ? 'bra' : spineC)

    this.onrenew = d => this.renew(I.renewExpr(d)(this.data))
    this.renewE  = d => this.renew(d)
    this.apply   = i => d => this.renew(I.fillExprWith(i)(d)(this.data))
    this.addArg  = d => (opts.addarg || this.apply(0))(d || I.eempty)

    this.onremove = () => {
        console.log('remove');
        this.renew(I.eempty)
    }
    this.ondrop = d => {
      console.log('drop', d);
      this.renew(I.assignExpr(this.data)(d))
    }
  </script>
</expr>

<expr-emp class='term emp'>
  <button if={opts.delete} onclick={opts.delete} class='remove'>×</button>
  <type data={data}/>
  <div class='area'></div>
  this.mixin(Mixin.Data)
</expr-emp>

<var-expr class='term var'>
  <span class='token'>{opts.data.value0}</span>
  this.mixin(Mixin.Data)
</var-expr>

<expr-lambda class='term lam'>
  <div class='left'>
    <span class='token'>λ</span>
    <pattern each={d, i in params} data={d} renew={renewA(i)}/>
    <add-area renew={addArg} name='expr'/>
    <span class='token'>→</span>
  </div>
  <expr data={expr} hole={true} right={true}/>

  <script>
    this.mixin(Mixin.Data)
    this.params = this.data.value0
    this.expr   = this.data.value1
    this.onrenew = d => this.renew(I.lambdaC(this.params)(d))
    this.renewA  = i => d => this.renew(d ? I.renewLambda(i)(d)(this.params)(this.expr)
                                          : I.deleteLambda(i)(this.params)(this.expr) )
    this.addArg  = d => this.renew(I.lambdaC(this.params.concat(d || I.pempty))(this.expr))
  </script>
</expr-lambda>

<app-expr class='term {opts.spine} {opts.outer?"outer":""}'>
  <expr data={data.value0} spine={opts.spine} left={true} renew={renewL} addarg={parent.addArg}/>
  <expr data={data.value1} bracket={true} right={opts.outer} renew={renewR} delete={removeR} fill={opts.spine}/>
  <script>
    this.mixin(Mixin.Data)
    this.renewL  = d => this.renew(I.appC(d)(this.data.value1))
    this.renewR  = d => this.renew(I.appC(this.data.value0)(d))
    this.removeR = () => this.opts.renewe(this.data.value0)
  </script>
</app-expr>

<expr-oper class='term opv {a||b?"":"just"}'>
  <div class='left'>
    <expr if={a} data={a} hole={true} left={true} renew={renewA}/>
    <hole if={!a} data={holes[0]} spine='opv' left={true} renew={renewA}/>
  </div>
  <oper-var data={o}/>
  <div class='right'>
    <expr if={b} data={b} hole={true} right={true} renew={renewB}/>
    <hole if={!b} data={holes[1]} spine='opv' right={true} renew={renewB}/>
  </div>

  <script>
    this.mixin(Mixin.Data)
    this.o = this.data.value0
    this.a = this.data.value1.value0
    this.b = this.data.value2.value0
    this.holes = I.arrowToArray(this.o.value1.value1); this.holes.pop()
    this.renewA  = a => this.renew(this.cons(this.o, a, this.b))
    this.renewB  = b => this.renew(this.cons(this.o, this.a, b))
    this.onrenew = o => this.renew(this.cons(o, this.a, this.b))
    this.cons = (o, a, b) => a ? b ? I.operC(o)(a)(b) : I.operCA(o)(a)
                               : b ? I.operCB(o)(b)   : I.operC0(o)
  </script>
</expr-oper>

<oper-var class='term opv'>
  <span class='token'>{expr.value0}</span>

  <infos show={hover} data={infos}/>
  <type-info show={hover} data={scheme}/>
  <highlight hover={hover} infos={infos}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.expr   = this.data.value0
    this.scheme = this.data.value1
    this.infos  = this.data.value2
  </script>
</oper-var>

<num-expr class='term num'>
  <handle/>
  <input-field data={data.value0}/>
  <!--<span class='token'>{opts.data.value0}</span>-->
  <script>
    this.mixin(Mixin.Data)
    this.onrenew = v => this.renew(I.numC(v))
  </script>
</num-expr>

<expr-if class='term ift'>
  <span class='token'>if</span>
  <expr data={c} hole={true}/>
  <span class='token'>then</span>
  <expr data={a} hole={true} renew={renewA}/>
  <span class='token'>else</span>
  <expr data={b} hole={true} right={true} renew={renewB}/>
  <script>
    this.mixin(Mixin.Data)
    this.c = this.data.value0
    this.a = this.data.value1
    this.b = this.data.value2
    this.onrenew = d => this.renew(I.ifC(d)(this.a)(this.b))
    this.renewA  = d => this.renew(I.ifC(this.c)(d)(this.b))
    this.renewB  = d => this.renew(I.ifC(this.c)(this.a)(d))
  </script>
</expr-if>

<expr-case class='term cas'>
  <div class='left'>
    <span class='token'>case</span>
    <expr data={a} hole={true}/>
    <span class='token'>of</span>
  </div>
  <div ref="list" class='list'>
    <case-alter each={d, i in as} data={d} renew={renewA(i)} remove={as1 && deleteA(i)}/>
  </div>
  <button onclick={addA} class='add'>+</button>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Sortable)
    this.name = 'alter'
    this.a   = this.data.value0
    this.as  = this.listData = this.data.value1
    this.as1 = this.as.length > 1
    this.onrenew = d => this.renew(I.caseC(d)(this.as))
    this.renewA  = i => d => this.renew(I.caseC(this.a)(I.renewI(i)(d)(this.as)))
    this.deleteA = i => d => this.renew(I.caseC(this.a)(I.deleteI(i)(this.as)))
    // this.renewP = i => d => this.renew(I.caseC(this.a)(I.renewFirsts(i)(d)(this.as)))
    // this.renewB = i => d => this.renew(I.caseC(this.a)(I.renewSeconds(i)(d)(this.as)))
    this.addA   = () => this.renew(I.caseC(this.a)(this.as.concat(I.aempty)))
    this.onsort = d => this.renew(I.caseC(this.a)(d))
  </script>
</expr-case>

<case-alter class={hover: hover}>
  <button if={opts.remove} onclick={opts.remove} class='remove'>×</button>
  <pattern data={p} outer={true} renew={renewP}/>
  <span class='token'>→</span>
  <expr data={b} outer={true}/>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.p = this.data.value0
    this.b = this.data.value1
    this.renewP  = d => this.renew(I.caseAltC(d)(this.b))
    this.onrenew = d => this.renew(I.caseAltC(this.p)(d))
  </script>
</case-alter>

<expr-let class='term let'>
  <span class='token'>let</span>
  <div ref='list' class='list sortable'>
    <bind each={d, i in binds} data={d} renew={renewB(i)}/>
  </div>
  <div class='expr'>
    <button onclick={addB} class='add'>+</button>
    <span class='token'>in</span>
    <expr data={expr} hole={true}/>
  </div>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Sortable)
    this.name = 'bind'
    this.binds = this.listData = this.data.value0
    this.expr  = this.data.value1
    this.renewB  = i => d => this.renew(I.letC(d ? I.renewI(i)(d)(this.binds)
                                                 : I.deleteI(i)(this.binds))
                                              (this.expr))
    this.onsort  = d => this.renew(I.letC(d)(this.expr))
    this.addB    = () => this.renew(I.letC(this.binds.concat(I.bempty))(this.expr))
    this.onrenew = d => this.renew(I.letC(this.binds)(d))
  </script>
</expr-let>

// Pattern ---------------------------------------------------------------------

<pattern class={outer: opts.outer, right: opts.right}>
  <div ref='slot' class='slot clonable'>
    <var-pattern if={cons == 'var'} data={expr}/>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Clonable)

    this.name   = 'expr'
    this.expr   = this.data.value0
    this.cons   = I.econs(this.expr)
    this.scheme = this.data.value1

    this.onrenew = d => {
        this.renew(d && I.renewExpr(d)(this.data))
    }
  </script>
</pattern>

<var-pattern class='term var'>
  <handle remove={remove}/>
  <input-field data={data.value0}/>
  <!--<span class='token'>{data.value0}</span>-->
  <script>
    this.mixin(Mixin.Data)
    this.onrenew = s => this.renew(I.varC(s))
    this.remove  = () => this.renew(null)
  </script>
</var-pattern>

// Parts -----------------------------------------------------------------------

<hole class='{opts.spine} {opts.left?"left":""} {opts.right?"right":""} {conpact?"conpact":""}'>
  <div ref='slot' class='slot droppable' onclick={opts.renew && add}>
    <type data={opts.data}/>
    <hole-stud data={opts.data} color={opts.spine} fill={opts.fill} outer={true}/>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    if (opts.renew) this.mixin(Mixin.Droppable)
    this.name = 'expr'
    this.scheme  = I.spure(this.data)
    this.conpact = opts.conpact == null ? true : opts.conpact

    this.ondrop = d => this.renew(d)
    this.add    = () => this.renew(I.eempty)
  </script>
</hole>

<hole-stud class='{opts.color} {opts.outer?"outer":""}'>
  <div class='fill {opts.fill}'>
    <hole-stud each={d in args} data={d} color={parent.opts.color} fill={parent.opts.fill}/>
  </div>
  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    if (!opts.outer) this.mixin(Mixin.Selectable)
    this.type = this.data.value0
    this.scheme  = I.spure(this.data)
    this.args = I.arrowToArray(this.data)
    this.args.pop()
  </script>
</hole-stud>

<handle>
  <div class='grip'></div>
  <button if={opts.remove} onclick={opts.remove} class='remove'>×</button>
</handle>

<add-area onclick={add}>
  <div ref='slot' class='slot droppable'></div>
  <span class='token'>+</span>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Droppable)
    this.name = opts.name
    this.add = () => this.renew(null)
  </script>
</add-area>

<highlight class={hover: opts.hover, error: error}>
  <script>
    this.error = opts.infos && opts.infos.errors && opts.infos.errors.length > 0
  </script>
</highlight>

<input-field>
  <input ref='input' type='text' value={data} onchange={onchange} oninput={oninput}/>
  <script>
    this.mixin(Mixin.Data)
    this.on('mount', () => this.oninput())
    this.onchange = e => this.renew(this.refs.input.value)
    this.oninput  = e => this.refs.input.style.width = this.refs.input.value.length + 'ch'
  </script>
</input-field>
