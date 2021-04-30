#  Blazor security 

I have been playing with the security configuration for a blazor application lately. This is a brain dump on the topic. More specifically the context is a client side blazor application with a backend (backend for frontend) and a OIDC authorization server (Azure AD). The focus of this article is on the client side rather than on the server side, since that is the part that is less mature and maybe less understood. Regarding what the blazor framework does and does not is a bit of guessing from my side since it is not always clear from the documentation but it is my mental model at the time of writing.

One requirement on the application was support for single sign-on in an ordinary angular-client  (the old one) and a blazor client (the new one). In the suggested solution both clients shared the same backend. The key to enable single sign-on was that they both shared the same session cookie provided by the aspnet core backend. Essentially the cookie just contains the authentication token that the OIDC server provided during the login phase. This means that, as long as the blazor client can support the same OIDC flow, single sign-on is supported.

## The 	&lt;AuthorizedView&gt; component and the [Authorize] attribute

A page can be tagged using the ```Authorized``` attribute in blazor.

```blazor
@attribute [Authorize]
```

The attribute works in conjunction with the AuthorizedView or AuthorizeRouteView, which is a specialized version of the former that supports routing. 

```xml
<CascadingAuthenticationState>
    <Router AppAssembly="@typeof(Program).Assembly" PreferExactMatches="@true">
        <Found Context="routeData">
            <AuthorizeRouteView RouteData="@routeData" DefaultLayout="@typeof(MainLayout)">
                <NotAuthorized>
                    <RedirectToLogin />
                </NotAuthorized>
            </AuthorizeRouteView>
        </Found>
        <NotFound>
            <LayoutView Layout="@typeof(MainLayout)">
                <p>Sorry, there's nothing at this address.</p>
            </LayoutView>
        </NotFound>
    </Router>
</CascadingAuthenticationState>
```

The AuthorizedView will check the user authorization state before rendering a sub-component tagged with the ```Authorized``` attribute. Based on the authentication state if will do one of the following
  
* Render the ```<NotAuthorized>``` section - **if the user is not authorized**
* Render the requested page - **if the user is authorized**

In the example above, if the user is not authorized, the ```<RedirectToLogin>``` component will be rendered. It is rather simple:

```csharp
@inject NavigationManager Navigation
@code {
    protected override void OnInitialized()
    {
        var relativeUrl = "/" + Navigation.ToBaseRelativePath(Navigation.Uri);
        Navigation.NavigateTo($"login?returnUrl={Uri.EscapeDataString(relativeUrl)}");
    }
}
```

It redirect to the login page keeping track of the current page so that the application can return there after the login process.

## The login page

The login page is also straight forward.

```csharp
@page "/login"
@inject NavigationManager Navigation
@using Microsoft.AspNetCore.WebUtilities

<button @onclick="LoggingIn">Login</button>

@code {
    public string ReturnUrl { get; set; }

    protected override void OnInitialized()
    {
        var uri = Navigation.ToAbsoluteUri(Navigation.Uri);
        if (QueryHelpers.ParseQuery(uri.Query).TryGetValue("returnUrl", out var returnUrl))
            ReturnUrl = returnUrl;
    }

    private void LoggingIn(MouseEventArgs args)
    {
        if (ReturnUrl != null)
            Navigation.NavigateTo($"authorize/sign-in?returnUrl={Uri.EscapeDataString(ReturnUrl)}", true);
        else
            Navigation.NavigateTo($"authorize/sign-in", true);
    }
}
```

It provides a login button that, when pressed, calls the backend ```authorize/sign-in``` endpoint. It will also pass on the requested return url if such is provided. Again, this is so that the application can return to the correct page after the login process is finished.

## The Authorize endpoint

The authorize endpoint uses the aspnet core method ```Challenge``` to start the login process. The code is provided below:

```csharp
[AllowAnonymous]
[Route("[controller]")]
public class AuthorizeController : Controller
{
    [HttpGet("sign-in")]
    public IActionResult SignIn([FromQuery] string returnUrl)
    {
        string safeReturnUrl;
        if (!string.IsNullOrWhiteSpace(returnUrl) && Url.IsLocalUrl(returnUrl))
            safeReturnUrl = returnUrl;
        else
            safeReturnUrl = "index.html";

        var authenticationProperties = new AuthenticationProperties { RedirectUri = safeReturnUrl };
        return Challenge(authenticationProperties, OpenIdConnectDefaults.AuthenticationScheme);
    }
}
```

After having finished the login session the backend is configured to set a session cookie. The configuration is done in ```Startup.ConfigureServices```

```csharp
public class Startup
{
    public void ConfigureServices(IServiceCollection services)
    {
        ...
        services.AddSingleton<IConfigureOptions<CookieAuthenticationOptions>, MyCookieOptions>();
        ...
    }
}
```

Now the session is established and all calls from the blazor client application will automatically attach the session cookie (along with all other cookies) to the subsequent backend requests. That is just how browsers work. &#128512;

## The authentication state

One question remains - how does the blazor framework figure out the authentication state?

The answer is: it uses the ```AuthenticationStateProvider```. The default implementation is made for server side applications. It will retrieve check ```IHttpContextAccessor.HttpContext.User.Identity.IsAuthenticated```. However, that does not work for a client side blazor application. Luckily, we can roll our own:

```csharp
public class HostAuthenticationStateProvider : AuthenticationStateProvider
{
    public override async Task<AuthenticationState> GetAuthenticationStateAsync()
    {
        return new AuthenticationState(await GetUserInfoFromBackend());
    }

    private async Task<ClaimsPrincipal> GetUserInfoFromBackend()
    {
        ...
    }
}
```

The idea is to expose a user endpoint in the backend that, basically returns ```IHttpContextAccessor.HttpContext.User.Identity.IsAuthenticated```. It is a good idea to add a bit of caching so that the endpoint is not called all the time.

## Bonus: Handling unauthorized api calls

There is one caveat to the solution above; in a single page application the user might stay for quite some time on a page without triggering a new call to ```AuthenticationStateProvider.GetAuthenticationStateAsync()``` which would trigger a new login if the session expired. 

# References

* https://docs.microsoft.com/en-us/aspnet/core/blazor/security/?view=aspnetcore-5.0&tabs=visual-studio&viewFallbackFrom=aspnetcore-3.0
* https://damienbod.com/2021/03/08/securing-blazor-web-assembly-using-cookies/
